{-# LANGUAGE OverloadedStrings #-}

module TypeCheck where

import AST

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Ord

import qualified Data.List as L
import Prelude hiding (lookup)

(>>>) = flip ($)

trace = flip const

lookup key map = case M.lookup key map of
       Just v -> Right v
       Nothing -> Left $ show key ++ " not found in map: " ++ show map

toEither True = Right undefined
toEither False = Left "False"

fromMaybe onError Nothing = Left onError
fromMaybe _ (Just a) = Right a

confirmInt (NamedType i) = if i == "int"
	  		   then Right $ NamedType i
			   else Left $ show i ++ " is not an int"
confirmInt t = Left $ show t ++ " is not an int :35"

confirmIntString (NamedType t) = if t == "int" || t == "string"
		 	       	 then Right $ NamedType t
				 else Left $ show t ++ " is neither int nor string"
confirmIntString x = Left $ show x ++ " is neither int nor string"

typeCheckArith e1 e2 = do
	       trace ("confirmInt: " ++ show e1) $ confirmInt <$> typeCheck e1
	       trace ("confirmInt: " ++ show e2) $ confirmInt <$> typeCheck e2
	       trace "" $ return $ (NamedType "int")

-- Takes a type and resolves any type aliases
-- Since record types ignore structural equivalence, they are always
-- canonical
canonicalize :: Type -> ReaderT (M.Map Identifier Type) (Either String) Type
canonicalize (NamedType name) = if L.elem name rootTypes
	       	 then return (NamedType name)
		 else ask >>= (lift . lookup name) >>= canonicalize
	     where rootTypes = ["int", "string"]
canonicalize r@(RecType _) = return r
canonicalize (ArrType t) = ArrType <$> canonicalize t
canonicalize (FuncType args ret) = FuncType <$> mapM canonicalize args <*> canonicalize ret
canonicalize Top = return Top
canonicalize VoidT = return VoidT

mergeTypes :: String -> Type -> Type -> ReaderT (M.Map Identifier Type) (Either String) Type
-- If one of the arguments is nil (type Top), it does match a record
mergeTypes' _ r@(RecType ps) Top = return r
mergeTypes' _ Top r@(RecType ps) = return r
mergeTypes' msg t1 t2 = do
	   case t1 == t2 of
	   	False -> lift . Left $ msg ++ " Couldn't merge " ++ show t1 ++ " and " ++ show t2
		True -> lift $ Right t1
	   return t1
	   
mergeTypes msg t1 t2 = do
	   t1' <- canonicalize t1
	   t2' <- canonicalize t2
	   mergeTypes' msg t1' t2'
    
matchesType e t = do
	     _ <- mergeTypes "" <$> typeCheck e <*> t
	     return t

sameType :: Show a => Expr a -> Expr a -> ReaderT (M.Map Identifier Type) (Either String) Type
sameType e1 e2 = do
	 t1 <- typeCheck e1
	 t2 <- typeCheck e2
	 env <- ask
	 mergeTypes ("checking that " ++ show e1 ++ " and " ++ show e2 ++ " have the same type in environment " ++ show env) t1 t2

isInteger e = trace ("isInteger? " ++ show e) $ typeCheck e >>= (lift . confirmInt)

insertMany = flip $ foldl (\m (k,v) -> M.insert k v m)

string = NamedType "string"
int = NamedType "int"
standardLibraryTypes = M.fromList [
		     ("print",FuncType [string] VoidT),
		     ("flush",FuncType [] VoidT),
		     ("getchar",FuncType [] string),
		     ("ord",FuncType [string] int),
		     ("chr",FuncType [int] string),
		     ("size",FuncType [string] int),
		     ("substring", FuncType [string,int,int] string),
		     ("concat",FuncType [string,string] string),
		     ("not", FuncType [int] int),
		     ("exit", FuncType [int] VoidT)]

typeCheckTiger :: Show a => Expr a -> Either String Type
typeCheckTiger prog = runReaderT (typeCheck prog) standardLibraryTypes

-- typecheck takes an expression, and returns the type of the expression
-- if it has a valid type
typeCheck :: Show a => Expr a -> ReaderT (M.Map Identifier Type) (Either String) Type
typeCheck (LValueId i _) = ask >>= (lift . lookup i)
typeCheck (LValueField rec fieldName _) = do
--	  Nothing <- (error . show <$> ask)
	  recTy <- typeCheck rec >>= canonicalize
	  case recTy of
	       (RecType fields) -> lift
	       		$ fromMaybe ("No field " ++ show fieldName
			  	    ++ " in record " ++ show recTy)
			$ L.lookup fieldName fields
	       Top -> lift $ Left "Nil type"
	       t -> lift $ Left $ "Not a record: " ++ show t ++ " in " ++ show rec ++ "." ++ show fieldName
typeCheck (LValueSubscript arr subscript _) = do
	  subTy <- typeCheck subscript
	  arrTy <- typeCheck arr
	  case arrTy of
	       ArrType t -> (lift $ confirmInt subTy) >> return t
	       _ -> lift $ Left $ show arrTy ++ " is not an array type"
typeCheck (Nil _) = return Top
typeCheck (Seq es _) = mapM typeCheck es >>= (return . last)
typeCheck (Void _) = return VoidT
typeCheck (IntLit i _) = return $ NamedType "int"
typeCheck (StringLit _ _) = return $ NamedType "string"
typeCheck (Negation i _) = typeCheck i >>= (lift . confirmInt)
typeCheck (FunctionCall funcName args _) = do
	  FuncType paramTypes retType <- ask >>= (lift . lookup funcName)
	  argTypes <- mapM (typeCheck >=> canonicalize) args
	  paramTypes' <- mapM canonicalize paramTypes
	  if argsMatch paramTypes' argTypes
	     then return retType
	     else lift . Left $ "Argument types don't match in call of " ++ show funcName ++ "  Expected: " ++ show paramTypes' ++ "   Got: " ++ show argTypes
       	  where argsMatch p a = length p == length a && (and $ L.zipWith (==) p a)
typeCheck (Add e1 e2 _) = typeCheckArith e1 e2
typeCheck (Sub e1 e2 _) = typeCheckArith e1 e2
typeCheck (Mult e1 e2 _) = typeCheckArith e1 e2
typeCheck (Div e1 e2 _) = typeCheckArith e1 e2
typeCheck (Comp Eq e1 e2 _) = sameType e1 e2 >> (return $ NamedType "int")
typeCheck (Comp _ e1 e2 _) = do
	  t1 <- typeCheck e1
	  t2 <- typeCheck e2
	  lift $ confirmIntString t1
	  lift $ confirmIntString t2
	  if t1 == t2
	     then return t1
	     else lift $ Left "Can't compare values of different type"
typeCheck (And e1 e2 _) = typeCheckArith e1 e2
typeCheck (Or e1 e2 _) = typeCheckArith e1 e2
typeCheck (Record typeId fields _) = do
	  recType <- ask >>= (lift . lookup typeId)
	  case recType of
	       RecType params -> argsMatch fields params >> return recType
	       _ -> lift . Left $ show recType ++ " is not a record type"
	  where argsMatch fields params = do
	  		  let ls = length fields == length params
	  		  ts <- typesMatch
			     (map snd $ L.sortBy (comparing fst) fields)
			     (map snd $ L.sortBy (comparing fst) params)
			  if ls && ts
			     then lift $ Right undefined
			     else lift $ Left $ "Arguments don't match in creation of record " ++ show typeId
		typesMatch fields params = (==) params <$> mapM typeCheck fields
typeCheck (Array typeId len val _) = do
	  isInteger len
	  arrType <- ask >>= (lift . lookup typeId)
	  valType <- typeCheck val
	  mergeTypes ("Array of " ++ show valType) (ArrType valType) arrType
	  return arrType
typeCheck (Assignment lval rval _) = do
	  sameType lval rval
	  return VoidT
typeCheck (IfThenElse c t f _) = do
	  isInteger c
	  sameType t f
typeCheck (IfThen c t d) = do
	  isInteger c
	  sameType t $ Void d
typeCheck (While c a d) = do
	  isInteger c
	  sameType a $ Void d
typeCheck (For i start end body d) = do
	  isInteger start
	  isInteger end
	  local (M.insert i (NamedType "int")) $ typeCheck body
typeCheck (Break _) = return VoidT
typeCheck (Let ds e _) = letCheck (splitDeclarations ds) e

letCheck :: Show a => [[Decl a]] -> Expr a -> ReaderT (M.Map Identifier Type) (Either String) Type
letCheck (ts@(TypeDec _ _ _ _:_):ds) e = do
	 let bindings = map extractSigs ts
	 newEnv <- insertMany bindings <$> ask
	 local (const newEnv) $ letCheck ds e
	 where extractSigs (TypeDec _ i t _) = (i,t)
	       extractSigs _ = error "Encountered a non-type binding"
letCheck ((VarDec _ i ve _:vs):ds) e = do
	 t <- typeCheck ve
	 local (M.insert i t) $ letCheck (vs:ds) e
letCheck ((TVarDec _ i t v _:vs):ds) e = do
	 typeCheck v >>= mergeTypes ("Type of variable " ++ show i) t
	 local (M.insert i t) $ letCheck (vs:ds) e
letCheck (fs@(FunDec _ _ _ _ _:_):ds) e = letFCheck fs ds e
letCheck (fs@(TFunDec _ _ _ _ _ _:_):ds) e = letFCheck fs ds e
letCheck ([]:ds) e = letCheck ds e
letCheck [] e = typeCheck e
--letCheck ds e = error $ "Encountered unexpected pattern: ds=" ++ show ds ++ "\te=" ++ show e

letFCheck :: Show a => [Decl a] -> [[Decl a]] -> Expr a -> ReaderT (M.Map Identifier Type) (Either String) Type
letFCheck funcs ds e = do
	  let bindings = map extractSig funcs
	  newEnv <- insertMany bindings <$> ask
	  local (const newEnv) $ mapM typeCheckDecl funcs
	  local (const newEnv) $ letCheck ds e
	  where extractSig (FunDec _ i args _ _) = (i,FuncType (map snd args) VoidT)
	  	extractSig (TFunDec _ i args r _ _) = (i,FuncType (map snd args) r)
		typeCheckFun (FunDec _ _ _ _ e) = typeCheck e
		typeCheckFun (TFunDec _ i _ r e _) = typeCheck e >>= mergeTypes ("Return type of function " ++ show i)  r

splitDeclarations :: [Decl a] -> [[Decl a]]
splitDeclarations = L.groupBy declType
		  where declType (TypeDec _ _ _ _) (TypeDec _ _ _ _) = True
		  	declType (VarDec _ _ _ _) (VarDec _ _ _ _) = True
			declType (TVarDec _ _ _ _ _) (TVarDec _ _ _ _ _) = True
			declType (VarDec _ _ _ _) (TVarDec _ _ _ _ _) = True
			declType (TVarDec _ _ _ _ _) (VarDec _ _ _ _) = True
			declType (FunDec _ _ _ _ _) (FunDec _ _ _ _ _) = True
			declType (TFunDec _ _ _ _ _ _) (TFunDec _ _ _ _ _ _) = True
			declType (FunDec _ _ _ _ _) (TFunDec _ _ _ _ _ _) = True
			declType (TFunDec _ _ _ _ _ _) (FunDec _ _ _ _ _) = True
			declType _ _ = False

typeCheckDecl :: (Show a) => Decl a -> ReaderT (M.Map Identifier Type) (Either String) (M.Map Identifier Type)
typeCheckDecl (TypeDec _ i t _) = M.insert i t <$> ask
typeCheckDecl (VarDec _ i e _) = M.insert i <$> typeCheck e <*> ask
typeCheckDecl (TVarDec _ i t e _) = M.insert i <$> (typeCheck e >>= mergeTypes ("In variable declaration " ++ show i ++ "=" ++ show e) t) <*> ask
typeCheckDecl (FunDec _ i args body _) = do
	      bodyType <- local (insertMany args) $ typeCheck body
	      M.insert i (FuncType (map snd args) bodyType) <$> ask
typeCheckDecl (TFunDec _ i args rt body _) = do
	      bodyType <- local (insertMany args) $ typeCheck body
	      return . toEither $ bodyType == rt
	      M.insert i (FuncType (map snd args) bodyType) <$> ask