module Main where

import qualified Data.Map   as Map
import           Data.Function (on)
import           Data.Maybe (fromJust)

-- https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
import Control.Monad.Identity

main :: IO ()
main = putStrLn "a"

test :: IO ()
test = do
    let test_input =
          App (Lam "x" TInt (BinOps Add (Var "x") (Lit (LInt 1)))) (Lit (LInt 2))
    print $ evaluate Map.empty test_input

-- test_input = 
--   App (Lam "x" TInt (BinOps Add (Var "x") (Lit (LInt 1)))) (Lit (LInt 2))

-- incorrect
-- test_input = 
--   App (Lam "x" TInt (BinOps Add (Var "x") (Lit (LInt 1)))) (Lit (LBool True))

type Name = String

-- The STLC is based on the (untyped) lambda calculus.
-- And the syntax of the STLC consists of two things: terms and types
data Term
  = Lam Name Type Term -- Lambda introduces type
  | Var Name
  | App Term Term
  | Lit Lit
  | BinOps PrimOp Term Term
  deriving (Show)

data PrimOp = Add | Mul | Sub | Div
  deriving Show

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show)

data Value
  = VInt Int
  | VBool Bool
  | VClosure String Term Scope -- term that not yet evaulate
  deriving (Show)

data Type
  = TInt
  | TBool
  | TFunc Type Type -- function type
  deriving (Show, Eq)

type Scope = Map.Map String Value -- evaluated value

evaluate :: Scope -> Term -> Value
evaluate _env (Lit (LInt x))   = VInt x
evaluate _env (Lit (LBool x))  = VBool x
evaluate env (Var name)        = fromJust (Map.lookup name env)
evaluate env (Lam arg _ body)  = VClosure arg body env
evaluate env (App term1 term2) = apply (evaluate env term1) (evaluate env term2)
evaluate env (BinOps ops x y)  = binOps ops (evaluate env x) (evaluate env y)

apply :: Value -> Value -> Value
apply (VClosure n e clo) ex = evaluate (Map.insert n ex clo) e
apply _ _                   = error "Tried to apply non-closure"

binOps :: PrimOp -> Value -> Value -> Value
binOps Add (VInt x) (VInt y) = VInt (x + y)
binOps Sub (VInt x) (VInt y) = VInt (x - y)
binOps Mul (VInt x) (VInt y) = VInt (x * y)
binOps Div (VInt x) (VInt y) = VInt (x `quot` y)
binOps _   _        _        = error "Not a number"

---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------

type TyCheckM a = Identity a

runTyCheck :: TyCheckM a -> a
runTyCheck ev = runIdentity ev

data TypeError
    = TypeMismatch Type Type
    | NotAFunction Type
    | NotInScope Name
    deriving (Show)

type Environment = Map.Map Name Type -- type

extendEnv :: Name -> Type -> Environment -> Environment
extendEnv = Map.insert

lookupEnv :: Environment -> Name -> Type
lookupEnv env name = case Map.lookup name env of
  Just ty -> ty
  Nothing -> error . show $ NotInScope name

typeOf :: Environment -> Term -> TyCheckM Type
typeOf env (Lit (LInt _)) = return TInt
typeOf env (Lit (LBool _)) = return TBool
typeOf env (Lam name ty term) = do
  returnType <- typeOf (extendEnv name ty env) term
  return $ TFunc ty returnType
typeOf env (App term1 term2) = do
  t1 <- typeOf env term1
  t2 <- typeOf env term2
  case t1 of
    TFunc a b | a == t2 -> return b
              | otherwise -> error . show $ TypeMismatch a t2
    ty -> error . show $ NotAFunction ty
typeOf env (Var name) = return $ lookupEnv env name
typeOf env (BinOps _ x y) = do
  xt <- typeOf env x
  yt <- typeOf env y
  case (xt, yt) of
    (TInt, TInt) -> return TInt
    (TInt, yt')  -> error. show $ TypeMismatch TInt yt'
    (xt', TInt)  -> error . show $ TypeMismatch TInt xt'
    (xt', yt')   -> error $ ((++) `on` show) (TypeMismatch TInt yt') (TypeMismatch TInt xt')
