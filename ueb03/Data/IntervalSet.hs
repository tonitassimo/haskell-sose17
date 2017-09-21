module Data.IntervalSet
where

-- ----------------------------------------

-- a pair of Ints can be represent with closed intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
  | x1 <= x2 && y1 >= (x2-1) || x2 <= x1 && y2 >= (x1-1) = True
  | x1 >= x2 && y1 <= y2 || x2 >= x1 && y2 <= y1 = True
  | otherwise                                    = False


less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2) = y1 < (x2-1)


nullInterval :: Interval -> Bool
nullInterval (x, y) = x > y


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv []     = True
inv [x]    = not (nullInterval x)
inv (x:xs) = not (nullInterval x) && less x (head xs) && inv xs

-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval (x, y) []     = singleInterval x y
insertInterval (x, y) (i:is)
  | not (nullInterval (x, y)) && less (x, y) i   = (x, y) : i : is
  | not (nullInterval (x, y)) && overlap (x,y) i = insertInterval (merge (x, y) i) is 
  | otherwise                                    = i : insertInterval (x, y) is

fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList [] = []
fromIntervalList (x:xs) = insertInterval x (fromIntervalList xs)


-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

union :: IntervalSet -> IntervalSet -> IntervalSet
union [] [] = []
union [] ys = ys
union xs [] = xs
union (x:xs) ys = insertInterval x (union xs ys)


member :: Int -> IntervalSet -> Bool
member n []     = False
member n (x:xs) = n >= fst x && n <= snd x || member n xs


fromList :: [Int] -> IntervalSet
fromList [] = []
fromList xs = fromIntervalList ( map ( head . singleton ) xs )


toList :: IntervalSet -> [Int]
toList [] = []
toList (x:xs) = [fst x..snd x] ++ toList xs


-- ----------------------------------------
