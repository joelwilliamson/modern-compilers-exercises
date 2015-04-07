
module IO where

import Parse
import TypeCheck

import Data.Text.IO


parseFile fileName = do
	  contents <- Data.Text.IO.readFile fileName
	  return $ parseTiger contents

typeCheckFile fileName = do
	      contents <- Data.Text.IO.readFile fileName
	      let (Right ast) = parseTiger contents
	      return $ typeCheckTiger ast