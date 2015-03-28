
module Parse where

import qualified Lexer as L
import Text.Parsec
import Data.Text hiding (break)

import Control.Applicative hiding ((<|>),many)

import Prelude hiding (break,sequence)

type Identifier = Text

data CompareOp = Eq | Neq | Gt | Lt | Gte | Lte
               deriving (Show,Eq)

data Expr = LValueId Identifier
          | LValueField Expr Identifier
          | LValueSubscript Expr Expr
          | Nil
          | Seq [Expr] -- Has value of the last expr
          | Void -- () or let ... in end;
          | IntLit Integer
          | StringLit Text
          | Negation Expr
          | FunctionCall Identifier [Expr] -- This has a value if it is a function, none for procedure
          | Add Expr Expr | Sub Expr Expr
          | Mult Expr Expr | Div Expr Expr
          | Comp CompareOp Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Record Identifier [(Identifier,Expr)] -- typeid {id=exp,id=exp,...}
          | Array Identifier Expr Expr -- typeid [n] of v, duplicates v n times
          | Assignment Expr Expr -- Compounds share backing and are never released
          | IfThenElse Expr Expr Expr -- The second two exprs must have the same type
          | IfThen Expr Expr -- The second expr must not produce a value
          | While Expr Expr -- e2 produces no value
          | For Identifier Expr Expr Expr -- for id := e1 to e2 do e3
          | Break
          | Let [Decl] Expr -- let decs in exps end
            deriving (Show,Eq)

parseE = parse expression ""

expression :: Parsec Text u Expr
expression = letE <|> arithmetic
             <|> assignment <|> ifThenElse <|> ifThen
             <|> while <|> for <|> break

lvalue :: Parsec Text u Expr
lvalue = identifier >>= (rest . LValueId)
  where rest inner = (LValueField inner <$> (L.dot >> identifier) >>= rest)
                     <|> (LValueSubscript inner <$> L.brackets (expression) >>= rest)
                     <|> return inner

identifier :: Parsec Text u Identifier
identifier = (\(L.Identifier i) -> i) <$> L.identifier'

nil = L.nil' >> return Nil

sequence :: Parsec Text u Expr
sequence = Seq <$> L.parens (sepBy expression L.semi)

void = Void <$ try (L.lParen >> L.rParen)
int = L.int' >>= (\(L.Int i) -> return $ IntLit i)
stringLit = (\(L.StringLit s) -> StringLit s) <$> L.stringLit'

functionCall = do
  name <- try (identifier <* L.lParen)
  args <- sepBy expression L.comma
  _ <- L.rParen
  return $ FunctionCall name args
arithmetic = boolOr
  where negation = Negation <$> (L.minus >> factor)
        multiplicative = chainl1 (negation <|> factor) multiplicativeOps
          where multiplicativeOps = (L.times *> pure Mult) <|> (L.div *> pure Div)
        additive = chainl1 multiplicative additiveOps
          where additiveOps = (L.plus *> pure Add) <|> (L.minus *> pure Sub)
        comparison = chainl1 additive compareOps
          where compareOps = (L.eq *> pure (Comp Eq)) <|> (L.neq *> pure (Comp Neq))
                             <|> (L.gt *> pure (Comp Gt)) <|> (L.lt *> pure (Comp Lt))
                             <|> (L.gte *> pure (Comp Gte)) <|> (L.lte *> pure (Comp Lte))
        boolAnd = chainl1 comparison (L.and *> pure Or)
        boolOr = chainl1 boolAnd (L.or *> pure And)
factor = literal <|> sequence <|> functionCall <|> assignment
literal = int <|> void <|> record <|> array <|> stringLit
record = Record <$> try (identifier <* L.lBrace) <*> sepBy1 recordField L.comma <* L.rBrace
  where recordField = do
          id <- identifier
          _ <- L.eq
          expr <- expression
          return (id,expr)
array = try $ Array <$> (identifier <* L.lBrack) <*> expression <* L.rBrack <* L.reserved "of" <*> expression
assignment = (Assignment <$> try (lvalue <* L.assign') <*> expression) <|> lvalue
ifThenElse = ifThen >>= (\(IfThen cond e1) -> (L.else' >> expression >>= return . IfThenElse cond e1) <|> return (IfThen cond e1))
ifThen = do
  _ <- L.if'
  cond <- expression
  _ <- L.then'
  e <- expression
  return $ IfThen cond e
while = While <$> (L.while' *> expression) <* L.do' *> expression
for = For <$> (L.for' *> identifier) <*> (L.assign' *> expression) <*> (L.to' *> expression) <*> (L.do' *> expression)
break = Break <$ L.break'
letE = Let <$> (L.let' *> many decl <* L.in') <*> (expression <* L.end')


data Decl = TypeDec Identifier Type
          | VarDec Identifier Expr
          | TVarDec Identifier Identifier Expr -- id : tpye-id := expr
          | FunDec Identifier [(Identifier,Identifier)] Expr
          | TFunDec Identifier [(Identifier,Identifier)] Identifier Expr -- function id (a1:t1,a2:t2,...) :tr = expr
          deriving (Show,Eq)

data Type = NamedType Identifier
          | RecType [(Identifier,Identifier)] -- { f1:t1,f2:t2 ...}
          | ArrType Identifier
          deriving (Show,Eq)

parseD = parse decl ""

decl = typeDec <|> varDec <|> funDec
tyField = (,) <$> identifier <*> (L.colon *> identifier)
typeDec = TypeDec <$> (L.type' *> identifier) <*> (L.eq *> typeVal)
  where typeVal :: Parsec Text u Type
        typeVal = (NamedType <$> identifier)
                  <|> (RecType <$> L.braces (sepBy tyField L.comma))
                  <|> (ArrType <$> (L.array' *> (L.of' *> identifier)))
varDec = L.var' *> ((VarDec <$> try (identifier <* L.assign') <*> expression)
                    <|> (TVarDec <$> identifier <*> (L.colon *> identifier) <*> (L.assign' *> expression)))
funDec :: Parsec Text u Decl
funDec = do
  funId <- L.function' *> identifier
  args <- L.parens (sepBy tyField L.comma)
  TFunDec funId args <$> (L.colon *> identifier) <*> (L.eq *> expression)
    <|> FunDec funId args <$> (L.eq *> expression)

instance Eq ParseError where
  _ == _ = False

expressionTests = Prelude.and [
  parseE "int [3] of 4" == Right (Array "int" (IntLit 3) (IntLit 4)),
  parseE "int [3+2] of 4*21" == Right (Array "int" (Add (IntLit 3) (IntLit 2)) (Mult (IntLit 4) (IntLit 21))),
  parseE "point {x=7, y= 4}" == Right (Record "point" [("x",IntLit 7),("y",IntLit 4)]),
  parseE "3+4*6-7/2" == Right (Sub (Add (IntLit 3) (Mult (IntLit 4) (IntLit 6))) (Div (IntLit 7) (IntLit 2))),
  parseE "f(3)" == Right (FunctionCall "f" [IntLit 3]),
  parseE "f(point {x=3,y=4}))" == Right (FunctionCall "f" [Record "point" [("x",IntLit 3),("y",IntLit 4)]]),
  parseE "x := point { x = 7, y = -2}" == Right (Assignment (LValueId "x") (Record "point" [("x",IntLit 7),("y",Negation (IntLit 2))])),
  parseE "if 3 then 4" ==Right (IfThen (IntLit 3) (IntLit 4)),
  parseE "if 3 then 4 else 5" == Right (IfThenElse (IntLit 3) (IntLit 4) (IntLit 5)),
  parseE "y:=y+x" == Right (Assignment (LValueId "y") (Add (LValueId "y") (LValueId "x")))
  ]
  
declTests = Prelude.and [
  parseD "type point = {x:int, y:int}" == Right (TypeDec "point" (RecType [("x","int"),("y","int")])),
  parseD "type ai = array of int" == Right (TypeDec "ai" (ArrType "int")),
  parseD "function square (x:int) : int = x * x" == Right (TFunDec "square" [("x","int")] "int" (Mult (LValueId "x") (LValueId "x")))
  ]
