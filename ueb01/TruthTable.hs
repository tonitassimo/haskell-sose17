module TruthTable
where

import           Data.List (intercalate)


type TruthTable = [[Bool]]

-- | generate a truth table for n variables

truthTable :: Int -> TruthTable
truthTable n
  | n == 0    = [[]]
  | n < 0     = error "not defined for negative values"
  | otherwise = map (True:) table ++ map(False:) table
  where
    table = truthTable (n-1)

ppTruthTable :: TruthTable -> String
ppTruthTable tt
  = unlines (map ppRow tt)
  where
    ppRow r = intercalate " " (map (take 1 . show) r)

printTT :: TruthTable -> IO ()
printTT = putStrLn . ppTruthTable

-- ----------------------------------------
