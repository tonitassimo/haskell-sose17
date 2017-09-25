{-# LANGUAGE UnboxedTuples #-}

module WordCount where

-- for frequency counts
import qualified Data.Map           as M

import qualified Data.Char          as C

-- maybe uesful for sorting results
-- frequency count
import qualified Data.List          as L

-- the working horse
-- with predefined type Sum for (+,0) monoid 
import           Data.Monoid

-- the more efficient Text representation
-- than the native String type
import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import           System.Environment (getArgs)

-- ----------------------------------------
--
-- the whole main program

main :: IO ()
main
  = do -- (inp :_) <- getArgs
       text     <- T.readFile "example_simple.txt"
       writeResult "example_simple.txt" (processText text)
       return ()

-- --------------------

type Counters
  = (Sum Int,              -- line count
     (Sum Int,             -- word count
      (Sum Int,            -- char count
       (Max,               -- length longest line
        (Sum Int,          -- whitespace count
         (FrequencyCount,  -- word frequency
          ()))))))

-- --------------------
--
-- monoid for natural numbers and maximum

newtype Max
  = Max Int

instance Monoid Max where
  mempty  = Max 0
  mappend (Max a) (Max b) = Max (max a b)

-- --------------------

newtype FrequencyCount
  = FC (M.Map T.Text Int)
  deriving (Show) -- just for testing
           
instance Monoid FrequencyCount where
  mempty = FC M.empty
  mappend (FC a) (FC b) = FC (M.unionWith (+) a b)

-- smart constructor
singleFC :: T.Text -> FrequencyCount
singleFC w = FC (M.singleton w 1)

-- --------------------
--
-- the whole computation

processText :: T.Text -> Counters
processText t
  = mconcat ( map toCounters ( T.lines t ) )

-- process a single line
toCounters :: T.Text -> Counters
toCounters t = (Sum 1,
                 (Sum (length (T.words t)),
                   (Sum (T.length t),
                     (Max (T.length t),
                       (Sum (T.length (T.filter C.isSpace t)),
                         (mconcat (map singleFC (T.words t)),
                           ()))))))

-- --------------------
--
-- the boring formatting of the results

writeResult :: String -> Counters -> IO ()
writeResult f (Sum lc, (Sum wc, (Sum cc, (Max ml, (Sum sc, (fm, ()))))))
  = putStrLn $ unlines $
    [ "Statistics for " ++ show f
    , "lines        : " ++ fillI8 lc
    , "words        : " ++ fillI8 wc
    , "chars        : " ++ fillI8 (cc + lc)
    , "whitespace   : " ++ fillI8 (lc + sc)
    , "longest line : " ++ fillI8 ml
    , "chars/line   : " ++ fillI8 (div' cc lc)
    , ""
    , "frequency count"
    ]
    ++
    formatFC fm
  where
    div' _ 0 = 0
    div' x y = (x + y `div` 2) `div` y
    
    fillI8 = fillLeft 8 . show

    fillLeft n v
      = replicate ((n - m) `max` 0) ' ' ++ v
        where
          m = length v

    fillRight n v
      = v ++ replicate ((n - m) `max` 0) ' '
        where
          m = length v
          
    formatFC (FC m)
      = map fmtWord {- . take 200 -} . L.sortBy fcOrd . M.toList $ m
      where
        fcOrd (w1, c1) (w2, c2) = (0 - c1, w1) `compare` (0 - c2, w2)

        fmtWord (w, c) = fillRight 13 (T.unpack w) ++ ": " ++ fillI8 c

-- ----------------------------------------
