-- This is a lexer for the Tiger programming language, defined in
-- Appel, Modern Compiler Implementation, Appendix A

{-# LANGUAGE NoMonomorphismRestriction #-}

module Lexer where

import Control.Applicative((<$>),(<$),(<*),(*>))
import Data.Functor.Identity

import Text.Parsec
import qualified Text.Parsec.Token as T
import Data.Text

import Prelude hiding (div,and,or)

lexer:: T.GenTokenParser Text u Identity
lexer = T.makeTokenParser $ T.LanguageDef {
  T.commentStart = "/*",
  T.commentEnd = "*/",
  T.commentLine = "",
  T.nestedComments = True,
  T.identStart = letter,
  T.identLetter = alphaNum <|> char '_',
  T.opStart = oneOf "+-*/=<>&|:;,",
  T.opLetter = oneOf ">=", -- The only multi-character operators are <> >= <= :=
  T.reservedNames = ["type", "array", "var", "function", "let", "in", "end",
                   "nil", "of", "if", "then", "else", "while", "do", "for", "to",
                   "break"],
  T.reservedOpNames = ["+","-","*","/","=","<>", ">","<",">=","<=","&","|",":="],
  T.caseSensitive = True
  }

data Token = Identifier Text
           | Type | Array | Var | Function | Let | In | End | Nil | Of
           | If | Then | Else | While | Do | For | To | Break
           | Plus | Minus | Times | Div | Eq | Neq | Gt | Lt | Gte | Lte | And | Or
           | Assign
           | StringLit Text
           | Int Integer
           | Nat Integer
           | Comment Text
           | LBrack | RBrack | LBrace | RBrace | LParen | RParen
           | Semi | Colon | Comma
           deriving (Show,Eq)

reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
ws = T.whiteSpace lexer
dot = T.dot lexer >> return ()
brackets = T.brackets lexer
braces = T.braces lexer
parens = T.parens lexer


identifier' = (Identifier . pack) <$> T.identifier lexer
type' = Type <$ reserved "type"
array' = Array <$ reserved "array"
var' = Var <$ reserved "var"
function' = Function <$ reserved "function"
let' = Let <$ reserved "let"
in' = In <$ reserved "in"
end' = End <$ reserved "end"
of' = Of <$ reserved "of"
if' = If <$ reserved "if"
then' = Then <$ reserved "then"
else' = Else <$ reserved "else"
while' = While <$ reserved "while"
do' = Do <$ reserved "do"
for' = For <$ reserved "for"
to' = To <$ reserved "to"
break' = Break <$ reserved "break"
nil' = Nil <$ reserved "nil"
plus = Plus <$ reservedOp "+"
minus = Minus <$ reservedOp "-"
times = Times <$ reservedOp "*"
div = Div <$ reservedOp "/"
eq = Eq <$ reservedOp "="
neq = Neq <$ reservedOp "<>"
gt = Gt <$ reservedOp ">"
lt = Lt <$ reservedOp "<"
gte = Gte <$ reservedOp ">="
lte = Lte <$ reservedOp "<="
and = And <$ reservedOp "&"
or = Or <$ reservedOp "|"
assign' = Assign <$ reservedOp ":="
stringLit' = (StringLit . pack) <$> T.stringLiteral lexer
int' = Int <$> T.integer lexer
nat' = Nat <$> T.natural lexer
whitespaced c = ws *> char c <* ws
lBrack = LBrack <$ whitespaced '['
rBrack = RBrack <$ whitespaced ']'
lBrace = LBrace <$ whitespaced '{'
rBrace = RBrace <$ whitespaced '}'
lParen = LParen <$ whitespaced '('
rParen = RParen <$ whitespaced ')'
semi = Semi <$ reservedOp ";"
colon = Colon <$ reservedOp ":"
comma = Comma <$ reservedOp ","


lexLanguage :: Parsec Text u [Token]
lexLanguage = ws >> (many $ choice [
  identifier',
  type', array', var', function', let', in', end', of', if', then', else',
  while', do', for', to', break',
  plus, minus, times, div, eq, neq, gt, lt, gte, lte, and, or, assign',
  stringLit',
  int', nat',
  lBrack, rBrack, lParen, rParen
  ,semi, colon
  ])
