module DList
    (
    DList
    , fromList
    , toList
    , empty
    , append
    , cons
    , dfoldr
    ) where


import qualified Data.Sequence as Seq

newtype DList a = DL {
    unDL :: [a] -> [a]
    }

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)


fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []

empty :: DList a
empty = DL id

-- equivalent of the list type's (:) operator
cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)
infixr `cons`

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xs = foldr f z (toList xs)

safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
                (y:_) -> Just y
                _ -> Nothing


dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
    where go x xs = cons (f x) xs

instance Functor DList where
    fmap = dmap


instance Monoid (DList a) where
    mempty = empty
    mappend = append

instance Semigroup (DList a) where
    (<>)  = append

