module AST where

import Data.Text

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


data Decl = TypeDec Identifier Type
          | VarDec Identifier Expr
          | TVarDec Identifier Type Expr -- id : tpye-id := expr
          | FunDec Identifier [(Identifier,Type)] Expr
          | TFunDec Identifier [(Identifier,Type)] Type Expr -- function id (a1:t1,a2:t2,...) :tr = expr
          deriving (Show,Eq)

data Type = NamedType Identifier
          | RecType [(Identifier,Type)] -- { f1:t1,f2:t2 ...}
          | ArrType Type
	  | FuncType [Type] Type
	  | Top -- This is the type of nil
	  | VoidT -- typeof(())
          deriving (Show,Eq)
