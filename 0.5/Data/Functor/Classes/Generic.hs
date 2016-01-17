{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Functor.Classes.Generic
  ( liftEqDefault
  , liftCompareDefault
  ) where

import GHC.Generics
import Data.Functor.Classes

---------------------------------------------------------------------------------------
-- * Eq1
---------------------------------------------------------------------------------------

liftEqDefault :: (GEq1 (Rep1 f), Generic1 f) => (a -> b -> Bool) -> f a -> f b -> Bool
liftEqDefault f m n = gliftEq f (from1 m) (from1 n)

class GEq1 t where
  gliftEq :: (a -> b -> Bool) -> t a -> t b -> Bool

instance Eq c => GEq1 (K1 i c) where
  gliftEq _ (K1 c) (K1 d) = c == d

instance (GEq1 f, GEq1 g) => GEq1 (f :*: g) where
  gliftEq f (a :*: b) (c :*: d) = gliftEq f a c && gliftEq f b d

instance (Eq1 f, GEq1 g) => GEq1 (f :.: g) where
  gliftEq f (Comp1 m) (Comp1 n) = liftEq (gliftEq f) m n

instance (GEq1 f, GEq1 g) => GEq1 (f :+: g) where
  gliftEq f (L1 a) (L1 c) = gliftEq f a c
  gliftEq f (R1 b) (R1 d) = gliftEq f b d
  gliftEq f _ _ = False

instance GEq1 f => GEq1 (M1 i c f) where
  gliftEq f (M1 a) (M1 b) = gliftEq f a b

instance GEq1 U1 where
  gliftEq _ U1 U1 = True

instance GEq1 V1 where
  gliftEq _ !x = undefined

instance GEq1 Par1 where
  gliftEq f (Par1 a) (Par1 b) = f a b

---------------------------------------------------------------------------------------
-- * Ord1
---------------------------------------------------------------------------------------

liftCompareDefault :: (GOrd1 (Rep1 f), Generic1 f) => (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareDefault f m n = gliftCompare f (from1 m) (from1 n)

class GOrd1 t where
  gliftCompare :: (a -> b -> Ordering) -> t a -> t b -> Ordering

instance Ord c => GOrd1 (K1 i c) where
  gliftCompare _ (K1 c) (K1 d) = compare c d

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :*: g) where
  gliftCompare f (a :*: b) (c :*: d) = gliftCompare f a c `mappend` gliftCompare f b d

instance (Ord1 f, GOrd1 g) => GOrd1 (f :.: g) where
  gliftCompare f (Comp1 m) (Comp1 n) = liftCompare (gliftCompare f) m n

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :+: g) where
  gliftCompare f (L1 a) (L1 c) = gliftCompare f a c
  gliftCompare f L1{} R1{} = LT
  gliftCompare f R1{} L1{} = GT
  gliftCompare f (R1 b) (R1 d) = gliftCompare f b d

instance GOrd1 f => GOrd1 (M1 i c f) where
  gliftCompare f (M1 a) (M1 b) = gliftCompare f a b

instance GOrd1 U1 where
  gliftCompare _ U1 U1 = EQ

instance GOrd1 V1 where
  gliftCompare _ !x = undefined

instance GOrd1 Par1 where
  gliftCompare f (Par1 a) (Par1 b) = f a b
