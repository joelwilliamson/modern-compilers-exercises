-- Implement a simple program analyzer and interpreter for the straight-line
-- programming language. This exercise serves as an introduction to environments
-- (symbol tables mapping variable-names to information about the variables);
-- to abstract syntax (data structures representing the phrase structure of
-- programs); to recursion over tree data structures, useful in many parts of a
-- compiler; and to afunctional style of programming without assignment
-- statements.

import qualified Data.Map as M
import Data.Maybe(fromJust)
import Data.Ord(comparing)

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

type Id = String

data Binop = Plus | Minus | Times | Div

data Stm = CompoundStm Stm Stm
           | AssignStm Id Exp
           | PrintStm [Exp]
         
data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp Binop Exp
         | EseqExp Stm Exp

maxArgs :: Stm -> Int
maxArgs (CompoundStm s1 s2) = max (maxArgs s1) (maxArgs s2)
maxArgs (AssignStm _ e) = maxArgsE e
maxArgs (PrintStm es) = max (length es) (maximum $ map maxArgsE es)

maxArgsE (IdExp _) = 0
maxArgsE (NumExp _) = 0
maxArgsE (OpExp e _ e') = max (maxArgsE e) (maxArgsE e')
maxArgsE (EseqExp s e) = max (maxArgs s) (maxArgsE e)

evalExp :: Exp -> StateT (M.Map String Int) (Writer [String]) Int
evalExp (IdExp i) = fromJust <$> M.lookup i <$> get
evalExp (NumExp n) = return n
evalExp (OpExp e op e') = do
  v <- evalExp e
  v' <- evalExp e'
  return (case op of
    Plus -> v + v'
    Minus -> v - v'
    Times -> v * v'
    Div -> v `div` v')
evalExp (EseqExp s e) = do
  runStm s
  evalExp e

runStm :: Stm -> StateT (M.Map String Int) (Writer [String]) Int
runStm (CompoundStm s s') = runStm s >> runStm s'
runStm (AssignStm i e) = do
  val <- evalExp e
  modify (M.insert i val)
  return val
runStm (PrintStm es) = do
  vals <- forM es evalExp
  tell [unwords $ map show vals]
  return 0

runProgram :: Stm -> String
runProgram p = unlines . snd .
               runWriter $ runStateT (runStm p) M.empty

-- a:=5+3; b:=print(a,a-1),10*a; print b
-- expected: 8 7\n80
testProgram = CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
              (CompoundStm (AssignStm "b"
                            (EseqExp (PrintStm [
                                         IdExp "a",
                                         OpExp (IdExp "a") Minus (NumExp 1)]
                                     )
                             (OpExp (NumExp 10) Times (IdExp "a"))))
              $ PrintStm [IdExp "b"])
