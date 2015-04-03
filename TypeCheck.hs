

module TypeCheck where

import AST

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Ord

import qualified Data.List as L

(>>>) = flip ($)
toMaybe True = Just undefined
toMaybe False = Nothing

confirmInt (NamedType i) = if i == "int"
	  		   then Just $ NamedType i
			   else Nothing
confirmInt _ = Nothing

confirmIntString (NamedType t) = if t == "int" || t == "string"
		 	       	 then Just $ NamedType t
				 else Nothing

typeCheckArith e1 e2 = do
	       confirmInt <$> typeCheck e1
	       confirmInt <$> typeCheck e2
	       return $ (NamedType "int")

-- Takes a type and resolves any type aliases
-- Since record types ignore structural equivalence, they are always
-- canonical
canonicalize :: Type -> ReaderT (M.Map Identifier Type) Maybe Type
canonicalize (NamedType name) = if L.elem name rootTypes
	       	 then return (NamedType name)
		 else ask >>= (lift . M.lookup name) >>= canonicalize
	     where rootTypes = ["int", "string"]
canonicalize r@(RecType _) = return r
canonicalize (ArrType t) = ArrType <$> canonicalize t
canonicalize (FuncType args ret) = FuncType <$> mapM canonicalize args <*> canonicalize ret
canonicalize Top = return Top
canonicalize VoidT = return VoidT

mergeTypes :: Type -> Type -> ReaderT (M.Map Identifier Type) Maybe Type
mergeTypes t1 t2 = do
	   t1' <- canonicalize t1 -- Find canonical form
	   t2' <- canonicalize t2
	   lift . toMaybe $ t1' == t2'
	   return t1'
	       
matchesType e t = do
	     _ <- mergeTypes <$> typeCheck e <*> t
	     return t

sameType :: Expr -> Expr -> ReaderT (M.Map Identifier Type) Maybe Type
sameType e1 e2 = do
	 t1 <- typeCheck e1
	 t2 <- typeCheck e2
	 mergeTypes t1 t2

isInteger e = typeCheck e >>= (lift . confirmInt)

insertMany = flip $ foldl (\m (k,v) -> M.insert k v m)

typeCheckTiger prog = runReaderT (typeCheck prog) M.empty

-- typecheck takes an expression, and returns the type of the expression
-- if it has a valid type
typeCheck :: Expr -> ReaderT (M.Map Identifier Type) Maybe Type
typeCheck (LValueId i) = ask >>= (lift . M.lookup i)
typeCheck (LValueField rec fieldName) = do
	  recTy <- typeCheck rec
	  case recTy of
	       (RecType fields) -> lift $ L.lookup fieldName fields
	       _ -> lift Nothing
typeCheck (LValueSubscript arr subscript) = do
	  subTy <- typeCheck subscript
	  arrTy <- typeCheck arr
	  lift $ ArrType <$> (confirmInt subTy <* Just arrTy)
typeCheck Nil = return Top
typeCheck (Seq es) = mapM typeCheck es >>= (return . last)
typeCheck Void = return VoidT
typeCheck (IntLit _) = return $ NamedType "int"
typeCheck (StringLit _) = return $ NamedType "string"
typeCheck (Negation i) = typeCheck i >>= (lift . confirmInt)
typeCheck (FunctionCall funcName args) = do
	  Just (FuncType paramTypes retType) <- M.lookup funcName <$> ask
	  argTypes <- mapM typeCheck args
	  if argsMatch paramTypes argTypes
	     then return retType
	     else lift Nothing
       	  where argsMatch p a = length p == length a && (and $ L.zipWith (==) p a)
typeCheck (Add e1 e2) = typeCheckArith e1 e2
typeCheck (Sub e1 e2) = typeCheckArith e1 e2
typeCheck (Mult e1 e2) = typeCheckArith e1 e2
typeCheck (Div e1 e2) = typeCheckArith e1 e2
typeCheck (Comp _ e1 e2) = do
	  t1 <- typeCheck e1
	  t2 <- typeCheck e2
	  lift $ confirmIntString t1
	  lift $ confirmIntString t2
	  if t1 == t2
	     then return t1
	     else lift Nothing
typeCheck (And e1 e2) = typeCheckArith e1 e2
typeCheck (Or e1 e2) = typeCheckArith e1 e2
typeCheck (Record typeId fields) = do
	  recType <- ask >>= (lift . M.lookup typeId)
	  case recType of
	       RecType params -> argsMatch fields params >> return recType
	       _ -> lift Nothing
	  where argsMatch fields params = do
	  		  let ls = length fields == length params
	  		  ts <- typesMatch
			     (map snd $ L.sortBy (comparing fst) fields)
			     (map snd $ L.sortBy (comparing fst) params)
			  if ls && ts
			     then lift $ Just undefined
			     else lift Nothing
		typesMatch fields params = (==) params <$> mapM typeCheck fields
typeCheck (Array typeId len val) = do
	  isInteger len
	  arrType <- ask >>= (lift . M.lookup typeId)
	  valType <- typeCheck val
	  mergeTypes valType arrType
	  return $ ArrType arrType
typeCheck (Assignment lval rval) = do
	  sameType lval rval
	  return VoidT
typeCheck (IfThenElse c t f) = do
	  isInteger c
	  sameType t f
typeCheck (IfThen c t) = do
	  isInteger c
	  sameType t Void
typeCheck (While c a) = do
	  isInteger c
	  sameType a Void
typeCheck (For i start end body) = do
	  isInteger start
	  isInteger end
	  local (M.insert i (NamedType "int")) $ typeCheck body
typeCheck Break = return VoidT
typeCheck (Let [] e) = typeCheck e
typeCheck (Let (d:ds) e) = do
	  newEnv <- typeCheckDecl d
	  local (const newEnv) $ typeCheck (Let ds e)


typeCheckDecl :: Decl -> ReaderT (M.Map Identifier Type) Maybe (M.Map Identifier Type)
typeCheckDecl (TypeDec i t) = M.insert i t <$> ask
typeCheckDecl (VarDec i e) = M.insert i <$> typeCheck e <*> ask
typeCheckDecl (TVarDec i t e) = M.insert i <$> (typeCheck e >>= mergeTypes t) <*> ask
typeCheckDecl (FunDec i args body) = do
	      bodyType <- local (insertMany args) $ typeCheck body
	      M.insert i (FuncType (map snd args) bodyType) <$> ask
typeCheckDecl (TFunDec i args rt body) = do
	      bodyType <- local (insertMany args) $ typeCheck body
	      return . toMaybe $ bodyType == rt
	      M.insert i (FuncType (map snd args) bodyType) <$> ask