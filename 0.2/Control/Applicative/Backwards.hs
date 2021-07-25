{-# LANGUAGE CPP #-}

#ifndef HASKELL98
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
# if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
# elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
# endif
# if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
# endif
#endif
-- |
-- Module      :  Control.Applicative.Backwards
-- Copyright   :  (c) Russell O'Connor 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Making functors with an 'Applicative' instance that performs actions
-- in the reverse order.
--
-- NB: This module is only included in @lens@ for backwards compatibility with
-- @transformers@ versions before 3.0.
module Control.Applicative.Backwards where

import Data.Functor.Classes

import Prelude hiding (foldr, foldr1, foldl, foldl1)
import Control.Applicative
import Data.Foldable
import Data.Traversable

#ifndef HASKELL98
# ifdef GENERIC_DERIVING
import Generics.Deriving.Base
# elif __GLASGOW_HASKELL__ >= 702
import GHC.Generics
# endif
#endif

-- | The same functor, but with an 'Applicative' instance that performs
-- actions in the reverse order.
newtype Backwards f a = Backwards { forwards :: f a }

#ifndef HASKELL98
# if __GLASGOW_HASKELL__ >= 702 || defined(GENERIC_DERIVING)
-- Generic(1) instances for Backwards
instance Generic (Backwards f a) where
  type Rep (Backwards f a) = D1 D1'Backwards (C1 C1_0'Backwards (S1 S1_0_0'Backwards (Rec0 (f a))))
  from (Backwards x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Backwards x

instance Generic1 (Backwards f) where
  type Rep1 (Backwards f) = D1 D1'Backwards (C1 C1_0'Backwards (S1 S1_0_0'Backwards (Rec1 f)))
  from1 (Backwards x) = M1 (M1 (M1 (Rec1 x)))
  to1 (M1 (M1 (M1 x))) = Backwards (unRec1 x)

data D1'Backwards
data C1_0'Backwards
data S1_0_0'Backwards

instance Datatype D1'Backwards where
  datatypeName _ = "Backwards"
  moduleName _ = "Control.Applicative.Backwards"
#  if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#  endif

instance Constructor C1_0'Backwards where
  conName _ = "Backwards"
  conIsRecord _ = True

instance Selector S1_0_0'Backwards where
  selName _ = "forwards"
# endif
#endif

instance (Eq1 f) => Eq1 (Backwards f) where
    liftEq eq (Backwards x) (Backwards y) = liftEq eq x y
    {-# INLINE liftEq #-}

instance (Ord1 f) => Ord1 (Backwards f) where
    liftCompare comp (Backwards x) (Backwards y) = liftCompare comp x y
    {-# INLINE liftCompare #-}

instance (Read1 f) => Read1 (Backwards f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "Backwards" Backwards

instance (Show1 f) => Show1 (Backwards f) where
    liftShowsPrec sp sl d (Backwards x) =
        showsUnaryWith (liftShowsPrec sp sl) "Backwards" d x

instance (Eq1 f, Eq a) => Eq (Backwards f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (Backwards f a) where compare = compare1
instance (Read1 f, Read a) => Read (Backwards f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (Backwards f a) where showsPrec = showsPrec1

-- | Derived instance.
instance (Functor f) => Functor (Backwards f) where
    fmap f (Backwards a) = Backwards (fmap f a)
    {-# INLINE fmap #-}
    x <$ Backwards a = Backwards (x <$ a)
    {-# INLINE (<$) #-}

-- | Apply @f@-actions in the reverse order.
instance (Applicative f) => Applicative (Backwards f) where
    pure a = Backwards (pure a)
    {-# INLINE pure #-}
    Backwards f <*> Backwards a = Backwards (a <**> f)
    {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
    liftA2 f (Backwards m) (Backwards n) = Backwards $ liftA2 (flip f) n m
    {-# INLINE liftA2 #-}
#endif
#if MIN_VERSION_base(4,2,0)
    Backwards xs *> Backwards ys = Backwards (ys <* xs)
    {-# INLINE (*>) #-}
    Backwards ys <* Backwards xs = Backwards (xs *> ys)
    {-# INLINE (<*) #-}
#endif

-- | Try alternatives in the same order as @f@.
instance (Alternative f) => Alternative (Backwards f) where
    empty = Backwards empty
    {-# INLINE empty #-}
    Backwards x <|> Backwards y = Backwards (x <|> y)
    {-# INLINE (<|>) #-}

-- | Derived instance.
instance (Foldable f) => Foldable (Backwards f) where
    foldMap f (Backwards t) = foldMap f t
    {-# INLINE foldMap #-}
    foldr f z (Backwards t) = foldr f z t
    {-# INLINE foldr #-}
    foldl f z (Backwards t) = foldl f z t
    {-# INLINE foldl #-}
    foldr1 f (Backwards t) = foldr1 f t
    {-# INLINE foldr1 #-}
    foldl1 f (Backwards t) = foldl1 f t
    {-# INLINE foldl1 #-}

-- | Derived instance.
instance (Traversable f) => Traversable (Backwards f) where
    traverse f (Backwards t) = fmap Backwards (traverse f t)
    {-# INLINE traverse #-}
    sequenceA (Backwards t) = fmap Backwards (sequenceA t)
    {-# INLINE sequenceA #-}
