module Data.Expr.Proposition.Proof where

import           Data.Expr.Proposition.Constr
import           Data.Expr.Proposition.Eval
import           Data.Expr.Proposition.Substitute
import           Data.Expr.Proposition.Types
import           Data.Pretty

import           Data.List                        (intercalate)
import           Data.Maybe                       (listToMaybe)

-- ----------------------------------------

truthTable :: Int -> [[Bool]]
truthTable n
  | n == 0    = [[]]
  | n < 0     = error "not defined for negative values"
  | otherwise = map (True:) table ++ map(False:) table
  where
    table = truthTable (n-1)

-- compute a proof by generating a truth table,
-- iterate over all rows in the table
-- and substitute all variable by the values in a row
-- and evaluate the expression
-- if a single result is false
-- we have a counter example, else the expr
-- is a tautology

proof' :: Expr -> Maybe VarEnv
proof' e = (listToMaybe . foldr evalEnv []) envs
  where
    varNames                   = freeVars e                                   -- alle vorkommenden Variablen in der Expression bestimmen
    fullTable                  = map (map Lit) $ truthTable $ length varNames -- Wahrheitstafel entsprechend der Anzahl der Variablen generieren
    envs                       = map (zip varNames) fullTable                 -- alle möglichen Umgebungen basierend auf der Wahrheitstafel und den Bezeichnern der Variablen generieren 
    evalEnv env falseEnvs                                                     -- Hilfsfunktion 
      | eval $ substVars env e = falseEnvs
      | otherwise              = env:falseEnvs

proof :: Expr -> String
proof e
  = case proof' e of
     Nothing
       -> pretty e ++ " is a tautology"
     Just env
       -> pretty e ++ " isn't a tautology, " ++
          "a counter example is " ++ ppEnv env
  where
    ppEnv = intercalate ", " . map ppVar
    ppVar (i, v) = i ++ "=" ++ pretty v

-- ----------------------------------------
