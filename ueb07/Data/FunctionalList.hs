module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-- ----------------------------------------

fromList        :: [a] -> List a
fromList l      = (l++)

toList          :: List a -> [a]
toList l        = l []

empty           :: List a
empty           = \_ -> []

singleton       :: a -> List a
singleton e     = (e:)

-- (:) for functional lists
cons            :: a -> List a -> List a
cons e l        = singleton e . l

-- dual to cons
snoc            :: List a -> a -> List a
snoc empty e    = singleton e
snoc l e        = l . singleton e

-- (++) for functional lists
append          :: List a -> List a -> List a
append l1 l2    = l1 . l2

-- like concat for normal lists: foldr (++) []
concat          :: [List a] -> List a
concat []       = empty
concat (l:ls)   = append l (concat ls)
--concat          = P.foldr append empty

-- like map for normal lists: foldr ((:) . f) []
map             :: (a -> b) -> List a -> List b
map f           = fromList . P.map f . toList

-- foldr with foldr for normal lists
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n      = P.foldr op n . toList

-- head, tail, null, reverse
head            :: List a -> a
head            = P.head . toList

tail            :: List a -> List a
tail l          = P.tail . l

null            :: List a -> Bool
null            = P.null . toList

reverse         :: List a -> List a
reverse l       = P.reverse . l

-- ----------------------------------------
