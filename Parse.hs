{-# LANGUAGE OverloadedStrings #-}

module Parse where

import qualified Lexer as L
import AST

import Text.Parsec hiding (State)
import Data.Text hiding (break)
import Control.Monad.State hiding (void,sequence)
import Data.Functor.Identity(Identity)

import Control.Applicative hiding ((<|>),many)

import Prelude hiding (break,sequence)

parseTiger text = fst $ runState (runParserT (L.ws *> expression) () "" text) 0

parseE text = fst $ runState (runParserT expression () "" text) 0

expression = letE <|> arithmetic
             <|> assignment <|> ifThenElse <|> ifThen
             <|> while <|> for <|> break

lvalue = identifier >>= (rest . LValueId)
  where rest inner = (LValueField inner <$> (L.dot >> identifier) >>= rest)
                     <|> (LValueSubscript inner <$> L.brackets (expression) >>= rest)
                     <|> return inner

identifier = (\(L.Identifier i) -> i) <$> L.identifier'
nil = L.nil' >> return Nil

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
literal = int <|> void <|> record <|> array <|> stringLit <|> nil
record = Record <$> try (identifier <* L.lBrace) <*> sepBy1 recordField L.comma <* L.rBrace <*> pure ()
  where recordField = do
          id <- identifier
          _ <- L.eq
          expr <- expression
          return (id,expr)
array = try $ Array <$> (identifier <* L.lBrack) <*> expression <* L.rBrack <* L.reserved "of" <*> expression <*> pure ()
assignment = (Assignment <$> try (lvalue <* L.assign') <*> expression <*> pure ()) <|> lvalue
ifThenElse = do
	   (IfThen cond e1 a) <- ifThen
	   (do
                L.else'
                e2 <- expression
                return $ IfThenElse cond e1 e2 a)
	    <|> return (IfThen cond e1 a)
ifThen = do
  _ <- L.if'
  cond <- expression
  _ <- L.then'
  e <- expression
  return $ IfThen cond e ()
while = While <$> (L.while' *> expression) <*> (L.do' *> expression) <*> pure ()
for = For <$> (L.for' *> identifier) <*> (L.assign' *> expression) <*> (L.to' *> expression) <*> (L.do' *> expression) <*> pure ()
break = Break <$ L.break' <*> pure ()
letE = Let <$> (L.let' *> many decl <* L.in') <*> (expression <* L.end') <*> pure ()

getNextId :: Enum a => ParsecT s u (State a) a
getNextId = do
	  current <- get
	  put $ succ current
	  return current

parseD text = fst $ runState (runParserT decl () "" text) 0

decl :: ParsecT Text u (State UniqueId) (Decl ())
decl = typeDec <|> varDec <|> funDec
tyField = (,) <$> identifier <*> (L.colon *> namedType)
namedType :: ParsecT Text u (State UniqueId) Type
namedType = NamedType <$> identifier
typeDec = TypeDec <$> getNextId <*> (L.type' *> identifier) <*> (L.eq *> typeVal) <*> pure ()
  where typeVal :: ParsecT Text u (State UniqueId) Type
        typeVal = namedType
                  <|> (RecType <$> L.braces (sepBy tyField L.comma))
                  <|> (ArrType <$> (L.array' *> (L.of' *> namedType)))

varDec = L.var' *> ((VarDec <$> getNextId <*> try (identifier <* L.assign') <*> expression <*> pure ())
                    <|> (TVarDec <$> getNextId <*> identifier <*> (L.colon *> namedType) <*> (L.assign' *> expression) <*> pure ()))

funDec :: ParsecT Text u (State UniqueId) (Decl ())
funDec = do
  funId <- L.function' *> identifier
  args <- L.parens (sepBy tyField L.comma)
  TFunDec <$> getNextId <*> pure funId <*> pure args <*> (L.colon *> namedType) <*> (L.eq *> expression) <*> pure ()
    <|> FunDec <$> getNextId <*> pure funId <*> pure args <*> (L.eq *> expression) <*> pure ()

expressionTests = Prelude.and [
  parseE "int [3] of 4" == Right (Array "int" (IntLit 3 ()) (IntLit 4 ()) ()),
  parseE "int [3+2] of 4*21" == Right (Array "int" (Add (IntLit 3 ()) (IntLit 2 ()) ()) (Mult (IntLit 4 ()) (IntLit 21 ()) ()) ()),
  parseE "point {x=7, y= 4}" == Right (Record "point" [("x",IntLit 7 ()),("y",IntLit 4 ())] ()),
  parseE "3+4*6-7/2" == Right (Sub (Add (IntLit 3 ()) (Mult (IntLit 4 ()) (IntLit 6 ()) ()) ()) (Div (IntLit 7 ()) (IntLit 2 ()) ()) ()),
  parseE "f(3)" == Right (FunctionCall "f" [IntLit 3 ()] ()),
  parseE "f(point {x=3,y=4}))" == Right (FunctionCall "f" [Record "point" [("x",IntLit 3 ()),("y",IntLit 4 ())] ()] ()),
  parseE "x := point { x = 7, y = -2}" == Right (Assignment (LValueId "x" ()) (Record "point" [("x",IntLit 7 ()),("y",Negation (IntLit 2 ()) ())] ()) ()),
  parseE "if 3 then 4" ==Right (IfThen (IntLit 3 ()) (IntLit 4 ()) ()),
  parseE "if 3 then 4 else 5" == Right (IfThenElse (IntLit 3 ()) (IntLit 4 ()) (IntLit 5 ()) ()),
  parseE "y:=y+x" == Right (Assignment (LValueId "y" ()) (Add (LValueId "y" ()) (LValueId "x" ()) ()) ())
  ]
  
declTests = Prelude.and [
  parseD "type point = {x:int, y:int}" == Right (TypeDec 0 "point" (RecType [("x",NamedType "int"),("y",NamedType "int")]) ()),
  parseD "type ai = array of int" == Right (TypeDec 0 "ai" (ArrType $ NamedType"int") ()),
  parseD "function square (x:int) : int = x * x" == Right (TFunDec 0 "square" [("x",NamedType "int")] (NamedType "int") (Mult (LValueId "x" ()) (LValueId "x" ()) ()) ())
  ]
