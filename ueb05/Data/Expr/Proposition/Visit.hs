-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visit where

import Data.Expr.Proposition.Types

-- ----------------------------------------

data Visitor r
  = V { vLit    :: Bool  -> r
      , vVar    :: Ident -> r
      , vUnary  :: Op1   -> r -> r
      , vBinary :: Op2   -> r -> r -> r
      }

idExpr :: Visitor Expr
idExpr
  = V { vLit = Lit, vVar = Var, vUnary = Unary, vBinary = Binary }

visit :: Visitor r -> Expr -> r
visit visitor (Lit e)          = vLit visitor e
visit visitor (Var e)          = vVar visitor e
visit visitor (Unary op e)     = (vUnary visitor) op $ visit visitor e
visit visitor (Binary op l r)  = (vBinary visitor) op (visit visitor l) (visit visitor r)

-- ----------------------------------------
