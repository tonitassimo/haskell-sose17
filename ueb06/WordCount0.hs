{-# LANGUAGE UnboxedTuples #-}

module WordCount0 where

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

{- wie kann man Argumente übergeben? -}
main :: IO ()
main
  = do 
       -- (inp :_) <- getArgs
       text     <- T.readFile "example.txt"
       writeResult "example.txt" (processText text)
       return ()

-- --------------------

type Counters
  = (Sum Int,              -- line count
     (Sum Int,             -- word count
      (Sum Int,            -- char count
       ())))

-- --------------------
--
-- the whole computation

processText :: T.Text -> Counters
processText t
  = mconcat ( map toCounters ( T.lines t ) )

-- process a single line
toCounters :: T.Text -> Counters
toCounters l = ( Sum 1, ( Sum ( length ( T.words l ) ), ( Sum ( T.length l ), () ) ) )

-- --------------------
--
-- the boring formatting of the results

writeResult :: String -> Counters -> IO ()
writeResult f (Sum lc, (Sum wc, (Sum cc, ())))
  = putStrLn $ unlines $
    [ "Statistics for " ++ show f
    , "lines        : " ++ fillI8 lc
    , "words        : " ++ fillI8 wc
    , "chars        : " ++ fillI8 (cc + lc)
    ]
  where
    fillI8 = fillLeft 8 . show

    fillLeft n v
      = replicate ((n - m) `max` 0) ' ' ++ v
        where
          m = length v

-- ----------------------------------------
