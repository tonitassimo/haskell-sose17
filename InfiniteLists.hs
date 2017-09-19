module InfiniteLists where

-- | construct a name generator
--
-- names = ["a".."z", "a1".."z1", "a2".."z2", ...]

names :: [String]
names = combos ['a'..'z']
  where
  combos lst = map (:[]) lst

temp :: String
temp =  ['a'..'z'] ++ ['0'..'9']

-- | constructs the infinite sequence
-- of fibonacci numbers in linear time
--
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- ----------------------------------------
--
-- | naive prime number generator with
-- sieve of Eratosthenes and a recursive
-- sieve operation

primes :: [Integer]
primes = sieve [2..]
  where
  sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]
   

-- ----------------------------------------
--
-- | the hamiltonian sequence is the ordered sequence
-- of natural number which are multiples of 2 or 3 or 5
--
-- Implementation: the 3 lists of multiples of 2, 3 and 5
-- are merged together with a @merges@ function.
--
-- The direct solution is

hamilton' :: [Integer]
hamilton'
  = filter
    (\ i ->    i `mod` 2 == 0
            || i `mod` 3 == 0
            || i `mod` 5 == 0
    ) [0..]

-- | @hamilton@ by merging sequences

hamilton :: [Integer]
hamilton
  = merges [is2, is3, is5]
    where
      is2 = [ x | x <- [2..], x `mod` 2 == 0 ]
      is3 = [ x | x <- [3..], x `mod` 3 == 0 ]
      is5 = [ x | x <- [5..], x `mod` 5 == 0 ]

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y  = x : merge xs (y:ys)
  | x == y =     merge xs (y:ys)     
  | x > y  = y : merge (x:xs) ys

-- | @merges@ takes a list of lists of ascending integers
-- and merges these lists into a single sorted list without any duplicates
-- direct impl

merges :: [[Integer]] -> [Integer]
merges []   = []
merges (x:xs) = merge x (merges xs)

-- | @merges@ with a fold

merges' :: [[Integer]] -> [Integer]
merges' = undefined    -- after chapter about folds

-- ----------------------------------------
