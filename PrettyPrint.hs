{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Parse
import Data.Text hiding (head,map,concat)

(##) :: Text -> Text -> Text
(##) = Data.Text.append

prettyE :: Expr -> [Text]
prettyE (LValueId i) = [i]
prettyE (LValueField e i) = [(head.prettyE) e ## "." ## i]
prettyE (LValueSubscript e off) = [(head.prettyE) e ## "[" ## (head.prettyE) off ## "]"]
prettyE Nil = ["nil"]
prettyE (Seq es) = ("seq:":) $ concat $ map (map (cons '|') . prettyE) es
prettyE Void = ["()"]
prettyE (IntLit i) = [pack $ show i]
prettyE (StringLit s) = ["\""##s##"\""]
prettyE (Negation i) = map ("-"##) $ prettyE i
prettyE (FunctionCall name args) = [name ## "(" ## (foldl1 (##) $ map prettyE args)]

