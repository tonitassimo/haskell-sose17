module Data.Expr.Proposition.Substitute where

import Data.List      (union)

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env (Lit e)            = Lit e
substVars env (Var e)            = maybe (Var e) id (lookup e env)  --lookup e env
substVars env (Unary op e)       = Unary op (substVars env e)
substVars env (Binary op e1 e2)  = Binary op (substVars env e1) (substVars env e2)


freeVars :: Expr -> [Ident]
freeVars (Lit e)            = []
freeVars (Var e)            = [e]
freeVars (Unary op e)       = freeVars e
freeVars (Binary op e1 e2)  = union (freeVars e1) (freeVars e2)

-- ----------------------------------------
