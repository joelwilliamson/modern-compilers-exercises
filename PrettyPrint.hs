{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import AST
import Data.Text as T hiding (head,map,concat)
import Data.Text.IO as TIO


(##) :: Text -> Text -> Text
(##) = T.append

indentLines :: Char -> [Text] -> [Text]
indentLines c = map (T.cons c)

prettyPut = TIO.putStrLn . T.unlines . prettyE

opIndent op e1 e2 = op : indentLines '|' (prettyE e1 ++ prettyE e2)

prettyE :: Expr a -> [Text]
prettyE (LValueId i _) = [i]
prettyE (LValueField e i _) = [(head.prettyE) e ## "." ## i]
prettyE (LValueSubscript e off _) = [(head.prettyE) e ## "[" ## (head.prettyE) off ## "]"]
prettyE (Nil _) = ["nil"]
prettyE (Seq es _) = ("seq:":) $ concat $ map (map (cons '|') . prettyE) es
prettyE (Void _) = ["()"]
prettyE (IntLit i _) = [pack $ show i]
prettyE (StringLit s _) = ["\""##s##"\""]
prettyE (Negation i _) = map ("-"##) $ prettyE i
prettyE (FunctionCall name args _) = name : indentLines ',' (args >>= prettyE)
prettyE (Add e1 e2 _) = opIndent "+" e1 e2
prettyE (Sub e1 e2 _) = opIndent "-" e1 e2
prettyE (Mult e1 e2 _) = opIndent "*" e1 e2
prettyE (Div e1 e2 _) = opIndent "/" e1 e2
prettyE (Comp co e1 e2 _) = opIndent op e1 e2
	where op = case co of
	      	 Eq -> "="
		 Neq -> "<>"
		 Gt -> ">"
		 Lt -> "<"
		 Gte -> ">="
		 Lte -> "<="
prettyE (And e1 e2 _) = opIndent "&" e1 e2
prettyE (Or e1 e2 _) = opIndent "|" e1 e2
prettyE (Record id vars _) = ("record: " ## id) : indentLines '|' vars'
	where vars' = concat $ map (\(id,e) -> id ## " <- " : prettyE e) vars