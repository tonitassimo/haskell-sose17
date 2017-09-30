module TreeImplLeaves where

    import qualified ListImpl as L

    data Tree a = Empty
                | Leaf a
                | Bin (Tree a) (Tree a)
                deriving (Show)

    -- todo: wie wird eingefügt,
    --       wenn sich die Daten
    --       nur in den Blättern
    --       befinden?
    insert' :: Tree a -> a -> Tree a
    insert' Empty e     = Leaf e
    insert' (Leaf l) e  = Bin (Leaf l) (Leaf e)
    insert' (Bin l r) e
        | count' l <= count' r = insert' l e
        | otherwise            = insert' r e

    count' :: Tree a -> Int
    count' Empty     = 0
    count' (Leaf l)  = 1
    count' (Bin l r) = 1 + (count' l) + (count' r)

    createTestTree :: Num a => Tree a
    createTestTree = (Bin (Bin (Leaf 1) (Leaf 2)) (Bin (Leaf 3) (Leaf 4)))

    treeToList :: Tree Int -> L.List'
    treeToList Empty = L.Empty