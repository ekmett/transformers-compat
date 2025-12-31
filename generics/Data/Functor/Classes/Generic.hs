{-|
Module:      Data.Functor.Classes.Generic
Copyright:   (C) 2015-2016 Edward Kmett, Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions to generically derive 'C.Eq1', 'C.Ord1', 'C.Read1', and 'C.Show1'
instances from "Data.Functor.Classes".
-}
module Data.Functor.Classes.Generic
  ( -- * Options
    Options(..)
  , defaultOptions
  , latestGHCOptions
    -- * 'Eq1'
  , liftEqDefault
  , liftEqOptions
    -- * 'Ord1'
  , liftCompareDefault
  , liftCompareOptions
    -- * 'Read1'
  , liftReadsPrecDefault
  , liftReadsPrecOptions
    -- * 'Show1'
  , liftShowsPrecDefault
  , liftShowsPrecOptions
    -- * 'GenericFunctorClasses'
  , FunctorClassesDefault(..)
    -- * Example
    -- $example
  ) where

import qualified Data.Functor.Classes as C ()
import           Data.Functor.Classes.Generic.Internal

{- $example
The most straightforward way to use the defaults in this module is to use
@DerivingVia@ on GHC 8.6 or later. For example:

@
&#123;-&#35; LANGUAGE DeriveGeneric, DerivingVia &#35;-&#125;

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics

data Pair a = Pair a a
  deriving stock Generic1
  deriving (Eq1, Ord1, Read1, Show1)
           via FunctorClassesDefault Pair
@

If using an older version of GHC, then one can also define instances manually.
Here is an example:

@
&#123;-&#35; LANGUAGE DeriveGeneric &#35;-&#125;

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics

data Pair a = Pair a a deriving Generic1

instance 'C.Eq1' Pair where
    'C.liftEq' = 'liftEqDefault'

instance 'C.Ord1' Pair where
    'C.liftCompare' = 'liftCompareDefault'

instance 'C.Read1' Pair where
    'C.liftReadsPrec' = 'liftReadsPrecDefault'

instance 'C.Show1' Pair where
    'C.liftShowsPrec' = 'liftShowsPrecDefault'
@
-}
