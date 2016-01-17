{-# LANGUAGE CPP #-}

#ifndef HASKELL98
# if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
# endif
# if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
# endif
#endif
-- |
-- Module      :  Data.Functor.Reverse
-- Copyright   :  (c) Russell O'Connor 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Making functors whose elements are notionally in the reverse order
-- from the original functor.
--
-- /NB:/ Note this module is only included in @lens@ for backwards
-- compatibility with older @containers@ versions.

module Data.Functor.Reverse where

import Control.Applicative.Backwards
import Data.Functor.Classes

import Prelude hiding (foldr, foldr1, foldl, foldl1)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

-- | The same functor, but with 'Foldable' and 'Traversable' instances
-- that process the elements in the reverse order.
newtype Reverse f a = Reverse { getReverse :: f a }

instance (Eq1 f) => Eq1 (Reverse f) where
    liftEq eq (Reverse x) (Reverse y) = liftEq eq x y

instance (Ord1 f) => Ord1 (Reverse f) where
    liftCompare comp (Reverse x) (Reverse y) = liftCompare comp x y

instance (Read1 f) => Read1 (Reverse f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "Reverse" Reverse

instance (Show1 f) => Show1 (Reverse f) where
    liftShowsPrec sp sl d (Reverse x) =
        showsUnaryWith (liftShowsPrec sp sl) "Reverse" d x

instance (Eq1 f, Eq a) => Eq (Reverse f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Reverse f a) where compare = compare1
instance (Read1 f, Read a) => Read (Reverse f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Reverse f a) where showsPrec = showsPrec1

-- | Derived instance.
instance (Functor f) => Functor (Reverse f) where
    fmap f (Reverse a) = Reverse (fmap f a)

-- | Derived instance.
instance (Applicative f) => Applicative (Reverse f) where
    pure a = Reverse (pure a)
    Reverse f <*> Reverse a = Reverse (f <*> a)

-- | Derived instance.
instance (Alternative f) => Alternative (Reverse f) where
    empty = Reverse empty
    Reverse x <|> Reverse y = Reverse (x <|> y)

-- | Fold from right to left.
instance (Foldable f) => Foldable (Reverse f) where
    foldMap f (Reverse t) = getDual (foldMap (Dual . f) t)
    foldr f z (Reverse t) = foldl (flip f) z t
    foldl f z (Reverse t) = foldr (flip f) z t
    foldr1 f (Reverse t) = foldl1 (flip f) t
    foldl1 f (Reverse t) = foldr1 (flip f) t

-- | Traverse from right to left.
instance (Traversable f) => Traversable (Reverse f) where
    traverse f (Reverse t) =
        fmap Reverse . forwards $ traverse (Backwards . f) t
    sequenceA (Reverse t) =
        fmap Reverse . forwards $ sequenceA (fmap Backwards t)
