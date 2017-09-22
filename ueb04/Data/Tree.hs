{-# LANGUAGE DeriveDataTypeable #-}

-- ----------------------------------------

-- | binary tree with values at the leafs (Tip),
-- the branches (Bin) don't contain any further information,
-- the empty tree is represented by a special value Null

module Data.Tree
where

import           Prelude             hiding (foldl, foldr, head, tail, init, last)

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Foldable
import           Data.Monoid

-- ----------------------------------------

data Tree a
    = Null
    | Tip a
    | Bin (Tree a) (Tree a)
      deriving (Show, Data, Typeable)

-- | data type invariant

invTree :: Tree a -> Bool
invTree Null         = True
invTree (Tip x)      = True
invTree (Bin Null _) = False
invTree (Bin _ Null) = False
invTree (Bin l r)    = invTree l && invTree r

-- | smart constructor
bin :: Tree a -> Tree a -> Tree a
bin Null Null = Null
bin Null r    = r
bin l Null    = l
bin l r       = Bin l r

instance Functor Tree where
  fmap f Null      = Null
  fmap f (Tip x)   = Tip (f x)
  fmap f (Bin l r) = bin (fmap f l) (fmap f r)

instance Applicative Tree where
  pure  = undefined
  (<*>) = undefined

instance Monad Tree where
  return     = undefined
  _    >>= _ = undefined

instance Alternative Tree where
  empty = mzero   -- or Null
  (<|>) = mplus

instance MonadPlus Tree where
  mzero = undefined
  mplus = undefined

instance Monoid (Tree a) where
  mempty  = Null
  mappend = bin

-- fold elements like in a list from right to left
instance Foldable Tree where
  foldr f e Null      = e
  foldr f e (Tip x)   = f x e
  foldr f e (Bin l r) = foldr f (foldr f e r) l

-- ----------------------------------------
-- classical visitor

visitTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
visitTree e tf bf = visit'
  where
    visit' Null      = e
    visit' (Tip x)   = tf x
    visit' (Bin l r) = bf (visit' l) (visit' r)

-- special visitors

sizeTree :: Tree a -> Int
-- sizeTree = visitTree 0 (const 1) (+)
sizeTree = visitTree 0 (\x -> 1) (+)

minDepth, maxDepth :: Tree a -> Int
-- have to sub one as we dont want to count the root of the tree
minDepth = (visitTree 0 (\x -> 1) (\l r -> 1 + min l r)) - 1
maxDepth = (visitTree 0 (\x -> 1) (\l r -> 1 + max l r)) - 1

-- ----------------------------------------
-- access functions

viewL :: Tree a -> Maybe (a, Tree a)
viewL  Null      = Nothing
viewL (Tip x)    = Just (x, Null)
viewL (Bin l r)  = Just (x, bin t r)
  where
  (Just (x, t)) = viewL l

viewR :: Tree a -> Maybe (Tree a, a)
viewR Null       = Nothing
viewR (Tip x)    = Just (Null, x)
viewR (Bin l r)  = Just (bin t l, x)
  where
 (Just (t, x)) = viewR r

head :: Tree a -> a
head = maybe (error "head: empty tree") fst . viewL

tail :: Tree a -> Tree a
tail = maybe (error "tail: empty tree") snd . viewL

last :: Tree a -> a
last = maybe (error "last: empty tree") snd . viewR

init :: Tree a -> Tree a
init = maybe (error "init: empty tree") fst . viewR

-- ----------------------------------------
-- conversions to/from lists

-- | runs in O(n) due to the use of (:)
toList :: Tree a -> [a]
toList = foldr (:) []

-- | runs in O(n * log n) due to the use of (++)
toListSlow :: Tree a -> [a]
toListSlow = visitTree [] (:[]) (++)

-- | build a balanced tree
--
-- doesn't work for infinite lists

-- weak balancing criterion
fromList :: [a] -> Tree a
fromList []  = Null
fromList [x] = Tip x
fromList xs  = bin (fromList l) (fromList r)
  where
  -- split at half
  (l, r) = splitAt ( div ( length xs ) 2 ) xs

-- strong balancing criterion
fromList' :: [a] -> Tree a
fromList' []  = Null
fromList' [x] = Tip x
fromList' xs  = bin (fromList' l) (fromList' r)
  where
  (l, r) = splitAt ( div ( length xs ) 2 ) xs

-- list to the right
fromList'' :: [a] -> Tree a
fromList'' = foldr (\x t -> Tip x `bin` t) Null

-- list to the left
fromList''' :: [a] -> Tree a
fromList''' = foldl (\t x -> t `bin` Tip x) Null

-- runtime differences between fromList, fromList', fromList'', fromList'''?
-- differences in balancing quality?

-- ----------------------------------------
