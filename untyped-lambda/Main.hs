module Main where

import qualified Data.Map   as Map
import           Data.Maybe (fromJust)

main :: IO ()
main = test

test :: IO ()
test = do
    let test_input =
          App (Lam "x" (BinOps Add (Var "x") (Lit (LInt 1)))) (Lit (LInt 2))
    print $ evaluate Map.empty test_input

type Name = String

--        App
-- ~~~~~~~~~~~~~~~~~~
-- (Lam x x)(Lam y y)
--      ~   ~~~~~~~~~ Lam
--     Var
data Term
  = Lam Name Term
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

-- or Environment
type Scope = Map.Map String Value -- evaluated value

evaluate :: Scope -> Term -> Value
evaluate _env (Lit (LInt x))   = VInt x
evaluate _env (Lit (LBool x))  = VBool x
evaluate env (Var name)        = fromJust (Map.lookup name env)
evaluate env (Lam arg body)    = VClosure arg body env
evaluate env (App term1 term2) = apply (evaluate env term1) (evaluate env term2)
evaluate env (BinOps ops x y)  = binOps ops (evaluate env x) (evaluate env y)

apply :: Value -> Value -> Value
apply (VClosure n e clo) ex = evaluate (Map.insert n ex clo) e
apply __________________ __ = error "Tried to apply non-closure"

binOps :: PrimOp -> Value -> Value -> Value
binOps Add (VInt x) (VInt y) = VInt (x + y)
binOps Sub (VInt x) (VInt y) = VInt (x - y)
binOps Mul (VInt x) (VInt y) = VInt (x * y)
binOps Div (VInt x) (VInt y) = VInt (x `quot` y)
binOps _   _        _        = error "Not a number"
