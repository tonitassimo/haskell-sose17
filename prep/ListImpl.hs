module ListImpl where
    
    data List' = Empty
               | Cons Int List'
               deriving (Show)

    instance Eq List' where
        Empty == Empty             = True
        (Cons x xs) == (Cons y ys) = 
            
            length' (Cons x xs) == length' (Cons y ys) &&
            x == y &&
            xs == ys
            

    length' :: List' -> Int
    length' Empty        = 0
    length' (Cons l lst) = 1 + length' lst

    insert' :: Int -> List' -> List'
    insert' x lst = Cons x lst

    insertSorted :: Int -> List' -> List'
    insertSorted e Empty = Cons e Empty
    insertSorted e (Cons l lst)
        | e <= l    = Cons e (Cons l lst)
        | otherwise = Cons l (insertSorted e lst)

    insertLast :: Int -> List' -> List'
    insertLast e Empty          = Cons e Empty    
    insertLast e (Cons l lst)   = Cons l (insertLast e lst)
        

    elem' :: Int -> List' -> Bool
    elem' e Empty        = False
    elem' e (Cons x lst)
          | x == e    = True
          | otherwise = elem' e lst 

    remove' :: Int -> List' -> List'
    remove' e lst   = if elem' e lst 
                         then removeElem e lst
                         else lst
                      where
                         removeElem e (Cons l lst)
                            | e == l    = lst
                            | otherwise = Cons l (removeElem e lst)

    -- Liste 2 an Liste 1 anhängen
    concat' :: List' -> List' -> List'
    concat' Empty lst      = lst
    concat' lst Empty      = lst
    concat' (Cons x xs) ys
        | xs == Empty = Cons x ys
        | otherwise   = Cons x (concat' xs ys)

    -- Duplikate entfernen
    removeDups :: List' -> List'
    removeDups Empty          = Empty
    removeDups (Cons x Empty) = Cons x Empty
    removeDups (Cons x (Cons y lst))
        | x == y    = Cons x (removeDups lst)
        | otherwise = Cons x (removeDups (Cons y lst))

    map' :: List' -> (Int -> Int) -> List'
    map' Empty _          = Empty
    map' (Cons l lst) f   = Cons (f l) (map' lst f)

    squares :: List' -> List'
    squares lst = map' lst (\x -> x * x)

    sum' :: List' -> Int
    sum' Empty        = 0
    sum' (Cons l lst) = l + (sum' lst)

    fold :: List' -> (Int -> Int -> Int) -> Int -> Int
    fold Empty op e        = e
    fold (Cons l lst) op e = op l (fold lst op e)

    lengthWithFold :: List' -> Int
    lengthWithFold lst = fold lst (\_ n -> n + 1) 0

    filter' :: List' -> (Int -> Bool) -> List'
    filter' Empty _           = Empty
    filter' (Cons l lst) cond
        | cond l    = Cons l (filter' lst cond)
        | otherwise = filter' lst cond

    even' :: List' -> List'
    even' lst = filter' lst (\n -> mod n 2 == 0)
    
