module AST where

import Data.Text

type Identifier = Text

data CompareOp = Eq | Neq | Gt | Lt | Gte | Lte
               deriving (Show,Eq)

data Expr a = LValueId Identifier a
            | LValueField (Expr a) Identifier a
          | LValueSubscript (Expr a) (Expr a) a
          | Nil a
          | Seq [Expr a] a -- Has value of the last expr
          | Void a -- () or let ... in end;
          | IntLit Integer a
          | StringLit Text a
          | Negation (Expr a) a
          | FunctionCall Identifier [Expr a] a -- This has a value if it is a function, none for procedure
          | Add (Expr a) (Expr a) a | Sub (Expr a) (Expr a) a
          | Mult (Expr a) (Expr a) a | Div (Expr a) (Expr a) a
          | Comp CompareOp (Expr a) (Expr a) a
          | And (Expr a) (Expr a) a
          | Or (Expr a) (Expr a) a
          | Record Identifier [(Identifier,Expr a)] a -- typeid {id=exp,id=exp,...}
          | Array Identifier (Expr a) (Expr a) a -- typeid [n] of v, duplicates v n times
          | Assignment (Expr a) (Expr a) a -- Compounds share backing and are never released
          | IfThenElse (Expr a) (Expr a) (Expr a) a -- The second two exprs must have the same type
          | IfThen (Expr a) (Expr a) a -- The second expr must not produce a value
          | While (Expr a) (Expr a) a -- e2 produces no value
          | For Identifier (Expr a) (Expr a) (Expr a) a -- for id := e1 to e2 do e3
          | Break a
          | Let [Decl a] (Expr a) a -- let decs in exps end
            deriving (Show,Eq)

type UniqueId = Integer

data Decl a = TypeDec UniqueId Identifier Type a
          | VarDec UniqueId Identifier (Expr a) a
          | TVarDec UniqueId Identifier Type (Expr a) a -- id : tpye-id := expr
          | FunDec UniqueId Identifier [(Identifier,Type)] (Expr a) a
          | TFunDec UniqueId Identifier [(Identifier,Type)] Type (Expr a) a -- function id (a1:t1,a2:t2,...) :tr = expr
          deriving (Show,Eq)

data Type = NamedType Identifier
          | RecType [(Identifier,Type)] -- { f1:t1,f2:t2 ...}
          | ArrType Type
	  | FuncType [Type] Type
	  | Top -- This is the type of nil
	  | VoidT -- typeof(())
          deriving (Show,Eq)
