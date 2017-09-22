-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------

type Idents = Set Ident

freeVarVisitor :: Visitor Idents
freeVarVisitor = V { vLit    = const S.empty
                   , vVar    = S.singleton
                   , vUnary  = const id
                   , vBinary = const S.union
                   }

freeVars :: Expr -> Idents
freeVars
  = visit $ freeVarVisitor


type VarEnv = [(Ident, Expr)]


substVarsVisitor :: VarEnv -> Visitor Expr
substVarsVisitor env = V { vLit    = Lit
--maybe (Var e) id (lookup e env)
                         , vVar    = (\ident -> maybe (Var ident) id (lookup ident env))
                         , vUnary  = Unary
                         , vBinary = Binary
                         }

substVars :: VarEnv -> Expr -> Expr
substVars
  = visit . substVarsVisitor


evalVisitor :: Visitor Bool
evalVisitor = V { vLit = id
                , vVar = (\name -> error $ unwords ["free variable" , show name, "in expression"])
                , vUnary = mf1
                , vBinary = mf2
                }

eval :: Expr -> Bool
eval
  = visit evalVisitor

-- ----------------------------------------
