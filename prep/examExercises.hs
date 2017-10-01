module Exercises where

    elem1 :: Eq a => a -> [a] -> Bool
    elem1 e [] = False
    elem1 e xs = found ( filter (\v -> v == e) xs )
        where
            found xs
                | xs == []  = False
                | otherwise = True
    
    -- todo
    elem2 :: Eq a => a -> [a] -> Bool
    elem2 e xs = undefined --foldr (\_ x -> e == x) False xs

    elem3 :: Eq a => a -> [a] -> Bool
    elem3 e xs = undefined

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 f xs = xs

    data T a = T0 a
             | T1 Op1 (T a)
             | T2 Op2 (T a) (T a)
    data Op1 = U1 | U2 | U3 deriving (Eq)
    data Op2 = B1 | B2 | B3 deriving (Eq)

    instance Eq a => Eq (T a) where
        (==) = eqT

    eqT :: Eq a => T a -> T a -> Bool
    eqT (T0 x) (T0 y)                   = x == y
    eqT (T1 op11 x) (T1 op12 y)         = op11 == op12 && x == y
    eqT (T2 op21 x1 y1) (T2 op22 x2 y2) = op21 == op22 && x1 == x2 && y1 == y2
    eqT _ _                             = False

    instance Functor T where
        fmap = mapT

    mapT :: (a -> b) -> T a -> T b
    mapT f (T0 x)       = T0 (f x)
    mapT f (T1 op1 x)   = T1 op1 (mapT f x)
    mapT f (T2 op2 x y) = T2 op2 (mapT f x) (mapT f y)

    zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith' f [] _          = []
    zipWith' f _ []          = []
    zipWith' f (x:xs) (y:ys) = [f x y] ++ (zipWith f xs ys)

    data Tree a = Nil
                | Fork a (Tree a) (Tree a)
                deriving (Show)

    zipTreeWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
    zipTreeWith f Nil _                           = Nil
    zipTreeWith f _ Nil                           = Nil
    zipTreeWith f (Fork v1 l1 r1) (Fork v2 l2 r2) = Fork (f v1 v2) (zipTreeWith f l1 l2) (zipTreeWith f r1 r2)

    -- todo
    merge' :: Ord a => [a] -> [a] -> [a]
    merge' [] ys = ys
    merge' xs [] = xs
    merge' (x:xs) (y:ys)
        | x < y  = x : y : (merge' xs ys)
        | x == y = x : (merge' xs ys)
        | x > y  = y : (merge' (x:xs) ys)

    -- todo
    halve :: [a] -> ([a], [a])
    halve []     = ([], [])
    halve [e]    = ([e], [])
    halve xs     = (xs, xs) --([x], [y])

    foldr' :: (a -> b -> b) -> b -> [a] -> b
    foldr' op e []     = e
    foldr' op e (x:xs) = op x (foldr' op e xs)

    prod' :: Num a => [a] -> a
    prod' xs = foldr' (*) 1 xs

    filter' :: (a -> Bool) -> [a] -> [a]
    filter' f [] = []
    filter' f xs = [x | x <- xs, f x]

    filter'' :: (a -> Bool) -> [a] -> [a]
    filter'' f [] = []
    filter'' f (x:xs)
        | f x = x : filter'' f xs
        | otherwise = filter'' f xs

    filter''' :: (a -> Bool) -> [a] -> [a]
    filter''' f [] = []
    filter''' f xs = foldr' (\x xs -> if f x then x:xs else xs) [] xs

    data Tree' a = Nil'
                | Leaf' a
                | Fork' a (Tree' a) (Tree' a)
                deriving (Show)

    find :: Eq a => a -> Tree' a -> Bool
    find e Nil'      = False
    find e (Leaf' x) = e == x
    find e (Fork' x l r)
        | e == x    = True
        | otherwise = find e l || find e r

    flatten :: Tree' a -> [a]
    flatten Nil'          = []
    flatten (Leaf' x)     = [x]
    flatten (Fork' x l r) = [x] ++ (flatten l) ++ (flatten r)
