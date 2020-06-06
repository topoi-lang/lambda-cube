module Main where

-- We will represent the variable context in Map,
-- and substitutions of type variables in Set
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.Except
import           Control.Monad.State

-- http://yinyanghu.github.io/posts/2014-03-13-algorithm-w.html
-- https://hackage.haskell.org/package/AlgorithmW

type Name = String
data Type
    = TVar Name
    | TFunc Type Type
    | TInt
    | TBool
    deriving (Show, Eq, Ord)

data Lit
    = LInt Int
    | LBool Bool deriving (Show)

data Term
    = Lit Lit
    | Var Name
    | App Term Term
    | Lam Name Term -- it is called abstraction sometimes (\lambda expression)
    | Let Name Term Term
    deriving (Show)

-- Type scheme
data Scheme = Scheme [Name] Type

type Subst = Map.Map Name Type

class Types a where
    -- free type variable
    ftv :: a -> Set.Set String
    -- apply substitution
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVar name)   = Set.singleton name
    ftv TInt          = Set.empty
    ftv TBool         = Set.empty
    ftv (TFunc t1 t2) = ftv t1 `Set.union` ftv t2

    apply s (TVar name) = case Map.lookup name s of
        Nothing -> TVar name
        Just t  -> t
    apply s (TFunc t1 t2) = TFunc (apply s t1) (apply s t2)
    apply s t             = t

instance Types Scheme where
    ftv (Scheme vars t) = (ftv t) `Set.difference` (Set.fromList vars)
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv list = foldr (Set.union . ftv) Set.empty list

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

-- The "𝚪" maps term to their type schemes, it is called gamma in some papers
newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

-- remove the binding of types
remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) varName = TypeEnv (Map.delete varName env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

-- fresh name for newly introduces type variables
data TIEnv = TIEnv {}
type TIState = Int

type TI a = ExceptT Name (State TIState) a

runTI :: TI a -> (Either Name a, TIState)
runTI t = runState (runExceptT t) initTIState where initTIState = 0

newTyVar :: TI Type
newTyVar = do
    s <- get
    put (s + 1)
    return (TVar (reverse (toTyVar s)))
  where
    toTyVar :: Int -> String
    toTyVar c | c < 26 = [toEnum (97 + c)]
              | otherwise = let (n, r) = c `divMod` 26 in (toEnum (97 + r)) : toTyVar (n - 1)

-- replaces all bound type variables in a type scheme with fresh type variables
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> newTyVar) vars
    let s = Map.fromList (zip vars nvars)
    return $ apply s t

-- algorithm U
-- unification function for types
mgu :: Type -> Type -> TI Subst
mgu (TFunc l r) (TFunc l' r')  = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu t1 t2 = throwError $ "types fo not unify: " ++ show t1 ++ " and " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u = return nullSubst
            | u `Set.member` ftv t = throwError $ "occur check fails: " ++ u ++ " and " ++ show t
            | otherwise = return (Map.singleton u t)

typeLit :: TypeEnv -> Lit -> TI (Subst, Type)
typeLit _ (LInt _) = return (nullSubst, TInt)
typeLit _ (LBool _) = return (nullSubst, TBool)

ti :: TypeEnv -> Term -> TI (Subst, Type)
ti (TypeEnv env) (Var name) = case Map.lookup name env of
    Nothing -> throwError $ "unbound variable: " ++ name
    Just sigma -> do
        t <- instantiate sigma
        return (nullSubst, t)
ti env (Lit lit) = typeLit env lit
ti env (Lam name term) = do
    tv <- newTyVar
    let TypeEnv env' = remove env name
    let env'' = TypeEnv (env' `Map.union` (Map.singleton name (Scheme [] tv)))
    (s1, t1) <- ti env'' term
    return (s1, TFunc (apply s1 tv) t1)
ti env (App term1 term2) = do
    tv <- newTyVar
    (s1, t1) <- ti env term1
    (s2, t2) <- ti (apply s1 env) term2
    s3 <- mgu (apply s2 t1) (TFunc t2 tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (Let name term1 term2) = do
    (s1, t1) <- ti env term1
    let TypeEnv env' = remove env name
    let t' = generalize (apply s1 env) t1
    let env'' = TypeEnv (Map.insert name t' env')
    (s2, t2) <- ti (apply s1 env'') term2
    return (s1 `composeSubst` s2, t2)

typeInference :: Map.Map Name Scheme -> Term -> TI Type
typeInference env term = do
    (s, t) <- ti (TypeEnv env) term
    return (apply s t)

test0, test1 :: Term
test0 = Let "id" (Lam "x" (Var "x")) (Var "id")
test1 = Let "id" (Lam "x" (Lit $ LInt 3)) (Var "id")

test :: Term -> IO ()
test term =
    let (result, _) = runTI (typeInference Map.empty term)
    in case result of
         Left err  ->  putStrLn $ show term ++ "\nerror: " ++ err
         Right t   ->  putStrLn $ show term ++ " :: " ++ show t

main :: IO ()
main = mapM_ test [test0, test1]