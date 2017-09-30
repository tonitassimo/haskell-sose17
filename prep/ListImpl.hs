module ListImpl where
    
    data List' a = Empty
                 | Cons a (List' a)
                 deriving (Show)

    instance Eq a => Eq (List' a) where
        Empty == Empty             = True
        (Cons x xs) == (Cons y ys) = 
            
            length' (Cons x xs) == length' (Cons y ys) &&
            x == y &&
            xs == ys
            

    length' :: List' a -> Int
    length' Empty        = 0
    length' (Cons l lst) = 1 + length' lst

    insert' :: Int -> List' Int -> List' Int
    insert' x lst = Cons x lst

    insertSorted :: Int -> List' Int -> List' Int
    insertSorted e Empty = Cons e Empty
    insertSorted e (Cons l lst)
        | e <= l    = Cons e (Cons l lst)
        | otherwise = Cons l (insertSorted e lst)

    insertLast :: Int -> List' Int -> List' Int
    insertLast e Empty          = Cons e Empty    
    insertLast e (Cons l lst)   = Cons l (insertLast e lst)
        

    elem' :: Int -> List' Int -> Bool
    elem' e Empty        = False
    elem' e (Cons x lst)
          | x == e    = True
          | otherwise = elem' e lst 

    remove' :: Int -> List' Int -> List' Int
    remove' e lst   = if elem' e lst 
                         then removeElem e lst
                         else lst
                      where
                         removeElem e (Cons l lst)
                            | e == l    = lst
                            | otherwise = Cons l (removeElem e lst)

    -- Liste 2 an Liste 1 anhängen
    concat' :: List' Int -> List' Int -> List' Int
    concat' Empty lst      = lst
    concat' lst Empty      = lst
    concat' (Cons x xs) ys
        | xs == Empty = Cons x ys
        | otherwise   = Cons x (concat' xs ys)

    -- Duplikate entfernen
    removeDups :: List' Int -> List' Int
    removeDups Empty          = Empty
    removeDups (Cons x Empty) = Cons x Empty
    removeDups (Cons x (Cons y lst))
        | x == y    = Cons x (removeDups lst)
        | otherwise = Cons x (removeDups (Cons y lst))

    map' ::  (a -> b) -> List' a -> List' b
    map' _ Empty        = Empty
    map' f (Cons l lst) = Cons (f l) (map' f lst)

    squares :: Num a => List' a -> List' a
    squares lst = map' (\x -> x * x) lst 

    sum' :: List' Int -> Int
    sum' Empty        = 0
    sum' (Cons l lst) = l + (sum' lst)

    fold :: List' Int -> (Int -> Int -> Int) -> Int -> Int
    fold Empty op e        = e
    fold (Cons l lst) op e = op l (fold lst op e)

    lengthWithFold :: List' Int -> Int
    lengthWithFold lst = fold lst (\_ n -> n + 1) 0

    filter' :: List' Int -> (Int -> Bool) -> List' Int
    filter' Empty _           = Empty
    filter' (Cons l lst) cond
        | cond l    = Cons l (filter' lst cond)
        | otherwise = filter' lst cond

    even' :: List' Int -> List' Int
    even' lst = filter' lst (\n -> mod n 2 == 0)

    -- Functor-Definition
    instance Functor List' where
        fmap = map'

    -- erste Funktion auf erstes Element anwenden etc.
    instance Applicative List' where
        pure                        = return
        _ <*> Empty                 = Empty
        Empty <*> _                 = Empty
        (Cons f fs) <*> (Cons v vs) = Cons (f v) (fs <*> vs)        

    instance Monad List' where
        return l  = Cons l Empty
        l >>= f   = _hole
