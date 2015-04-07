

module Escapes where

import AST
import Parse

import Data.Map as M
import Data.MultiMap as MM

import Control.Applicative
import Control.Monad.Reader

-- Generically, a variable escapes in three cases:
--    * It is passed by reference
--    * Its address is taken
--    * It is accessed from a nested function
-- In Tiger, records and arrays are always treated as references, but if they
-- are returned the backing data must be on the heap. Scalars can never be
-- referenced. We therefore only need to look for the case of a variable being
-- accessed in a nested function.
findEscapes :: Expr -> [UniqueId]
findEscapes e = runReader (findEscapes' e) (M.empty,MM.empty)

-- The first Reader will contain any variables that were declared **in the
-- current scope.** Thus, if a variable is encountered that is **not** in the
-- Reader, it must have escaped from somewhere. The second reader contains a
-- mapping between all in-scope variables and their Ids. The variable with the
-- greatest Id is the one with the closest scope.
findEscapes' :: Expr -> Reader (Map Identifier UniqueId, MultiMap Identifier UniqueId) [UniqueId]
findEscapes' (LValueId _) = ask >>= return . M.elems . fst
findEscapes' (LValueField rec _) = findEscapes' rec
findEscapes' (LValueSubscript arr sub) = (++) <$> findEscapes' arr <*> findEscapes' sub
findEscapes' Nil = return []
findEscapes' (Seq Expr) = 