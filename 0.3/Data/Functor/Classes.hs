{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(a,b,c) 1
#endif
-- |
-- Module      :  Data.Functor.Classes
-- Copyright   :  (c) Ross Paterson 2013, Edward Kmett 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Prelude classes, lifted to unary type constructors.

module Data.Functor.Classes (
    -- * Liftings of Prelude classes
    Eq1(..),
    Ord1(..),
    Read1(..),
    Show1(..),
    -- * Helper functions
    readsData,
    readsUnary,
    readsUnary1,
    readsBinary1,
    showsUnary,
    showsUnary1,
    showsBinary1,
  ) where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Monoid (Monoid(mappend))
#if MIN_VERSION_transformers(0,3,0)
import Control.Applicative.Lift
import Control.Applicative.Backwards
import Data.Functor.Reverse
#endif

instance Show a => Show (Identity a) where
  showsPrec d (Identity a) = showParen (d > 10) $
    showString "Identity " . showsPrec 11 a
instance Read a => Read (Identity a) where
  readsPrec d = readParen (d > 10) (\r -> [(Identity m,t) | ("Identity",s) <- lex r, (m,t) <- readsPrec 11 s])
instance Eq a   => Eq (Identity a) where
  Identity a == Identity b = a == b
instance Ord a  => Ord (Identity a) where
  compare (Identity a) (Identity b) = compare a b

instance Show a => Show (Constant a b) where
  showsPrec d (Constant a) = showParen (d > 10) $
    showString "Constant " . showsPrec 11 a
instance Read a => Read (Constant a b) where
  readsPrec d = readParen (d > 10) (\r -> [(Constant m,t) | ("Constant",s) <- lex r, (m,t) <- readsPrec 11 s])
instance Eq a   => Eq (Constant a b) where
  Constant a == Constant b = a == b
instance Ord a  => Ord (Constant a b) where
  compare (Constant a) (Constant b) = compare a b

-- | Lifting of the 'Eq' class to unary type constructors.
class Eq1 f where
    eq1 :: (Eq a) => f a -> f a -> Bool

-- | Lifting of the 'Ord' class to unary type constructors.
class (Eq1 f) => Ord1 f where
    compare1 :: (Ord a) => f a -> f a -> Ordering

-- | Lifting of the 'Read' class to unary type constructors.
class Read1 f where
    readsPrec1 :: (Read a) => Int -> ReadS (f a)

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    showsPrec1 :: (Show a) => Int -> f a -> ShowS

-- Instances for Prelude type constructors

instance Eq1 Maybe where eq1 = (==)
instance Ord1 Maybe where compare1 = compare
instance Read1 Maybe where readsPrec1 = readsPrec
instance Show1 Maybe where showsPrec1 = showsPrec

instance Eq1 [] where eq1 = (==)
instance Ord1 [] where compare1 = compare
instance Read1 [] where readsPrec1 = readsPrec
instance Show1 [] where showsPrec1 = showsPrec

instance (Eq a) => Eq1 ((,) a) where eq1 = (==)
instance (Ord a) => Ord1 ((,) a) where compare1 = compare
instance (Read a) => Read1 ((,) a) where readsPrec1 = readsPrec
instance (Show a) => Show1 ((,) a) where showsPrec1 = showsPrec

instance (Eq a) => Eq1 (Either a) where eq1 = (==)
instance (Ord a) => Ord1 (Either a) where compare1 = compare
instance (Read a) => Read1 (Either a) where readsPrec1 = readsPrec
instance (Show a) => Show1 (Either a) where showsPrec1 = showsPrec

-- Building blocks

-- | @'readsData' p d@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and
-- passes it to @p@.  Parsers for various constructors can be constructed
-- with 'readsUnary', 'readsUnary1' and 'readsBinary1', and combined with
-- @mappend@ from the @Monoid@ class.
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader d =
    readParen (d > 10) $ \ r -> [res | (kw,s) <- lex r, res <- reader kw s]

-- | @'readsUnary' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec'.
readsUnary :: (Read a) => String -> (a -> t) -> String -> ReadS t
readsUnary name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec 11 s]

-- | @'readsUnary1' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec1'.
readsUnary1 :: (Read1 f, Read a) => String -> (f a -> t) -> String -> ReadS t
readsUnary1 name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec1 11 s]

-- | @'readsBinary1' n c n'@ matches the name of a binary data constructor
-- and then parses its arguments using 'readsPrec1'.
readsBinary1 :: (Read1 f, Read1 g, Read a) =>
    String -> (f a -> g a -> t) -> String -> ReadS t
readsBinary1 name cons kw s =
    [(cons x y,u) | kw == name,
        (x,t) <- readsPrec1 11 s, (y,u) <- readsPrec1 11 t]

-- | @'showsUnary' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
showsUnary :: (Show a) => String -> Int -> a -> ShowS
showsUnary name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec 11 x

-- | @'showsUnary1' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
showsUnary1 :: (Show1 f, Show a) => String -> Int -> f a -> ShowS
showsUnary1 name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x

-- | @'showsBinary1' n d x@ produces the string representation of a binary
-- data constructor with name @n@ and arguments @x@ and @y@, in precedence
-- context @d@.
showsBinary1 :: (Show1 f, Show1 g, Show a) =>
    String -> Int -> f a -> g a -> ShowS
showsBinary1 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x .
        showChar ' ' . showsPrec1 11 y


instance (Eq e, Eq1 m, Eq a) => Eq (ErrorT e m a) where
    ErrorT x == ErrorT y = eq1 x y

instance (Ord e, Ord1 m, Ord a) => Ord (ErrorT e m a) where
    compare (ErrorT x) (ErrorT y) = compare1 x y

instance (Read e, Read1 m, Read a) => Read (ErrorT e m a) where
    readsPrec = readsData $ readsUnary1 "ErrorT" ErrorT

instance (Show e, Show1 m, Show a) => Show (ErrorT e m a) where
    showsPrec d (ErrorT m) = showsUnary1 "ErrorT" d m

instance (Eq e, Eq1 m) => Eq1 (ErrorT e m) where eq1 = (==)
instance (Ord e, Ord1 m) => Ord1 (ErrorT e m) where compare1 = compare
instance (Read e, Read1 m) => Read1 (ErrorT e m) where readsPrec1 = readsPrec
instance (Show e, Show1 m) => Show1 (ErrorT e m) where showsPrec1 = showsPrec

instance (Eq1 f, Eq a) => Eq (IdentityT f a) where
    IdentityT x == IdentityT y = eq1 x y

instance (Ord1 f, Ord a) => Ord (IdentityT f a) where
    compare (IdentityT x) (IdentityT y) = compare1 x y

instance (Read1 f, Read a) => Read (IdentityT f a) where
    readsPrec = readsData $ readsUnary1 "IdentityT" IdentityT

instance (Show1 f, Show a) => Show (IdentityT f a) where
    showsPrec d (IdentityT m) = showsUnary1 "IdentityT" d m

instance Eq1 f => Eq1 (IdentityT f) where eq1 = (==)
instance Ord1 f => Ord1 (IdentityT f) where compare1 = compare
instance Read1 f => Read1 (IdentityT f) where readsPrec1 = readsPrec
instance Show1 f => Show1 (IdentityT f) where showsPrec1 = showsPrec

instance (Eq1 m, Eq a) => Eq (ListT m a) where
    ListT x == ListT y = eq1 x y

instance (Ord1 m, Ord a) => Ord (ListT m a) where
    compare (ListT x) (ListT y) = compare1 x y

instance (Read1 m, Read a) => Read (ListT m a) where
    readsPrec = readsData $ readsUnary1 "ListT" ListT

instance (Show1 m, Show a) => Show (ListT m a) where
    showsPrec d (ListT m) = showsUnary1 "ListT" d m

instance Eq1 m => Eq1 (ListT m) where eq1 = (==)
instance Ord1 m => Ord1 (ListT m) where compare1 = compare
instance Read1 m => Read1 (ListT m) where readsPrec1 = readsPrec
instance Show1 m => Show1 (ListT m) where showsPrec1 = showsPrec

instance (Eq1 m, Eq a) => Eq (MaybeT m a) where
    MaybeT x == MaybeT y = eq1 x y

instance (Ord1 m, Ord a) => Ord (MaybeT m a) where
    compare (MaybeT x) (MaybeT y) = compare1 x y

instance (Read1 m, Read a) => Read (MaybeT m a) where
    readsPrec = readsData $ readsUnary1 "MaybeT" MaybeT

instance (Show1 m, Show a) => Show (MaybeT m a) where
    showsPrec d (MaybeT m) = showsUnary1 "MaybeT" d m

instance Eq1 m => Eq1 (MaybeT m) where eq1 = (==)
instance Ord1 m => Ord1 (MaybeT m) where compare1 = compare
instance Read1 m => Read1 (MaybeT m) where readsPrec1 = readsPrec
instance Show1 m => Show1 (MaybeT m) where showsPrec1 = showsPrec

instance (Eq w, Eq1 m, Eq a) => Eq (Lazy.WriterT w m a) where
    Lazy.WriterT x == Lazy.WriterT y = eq1 x y

instance (Ord w, Ord1 m, Ord a) => Ord (Lazy.WriterT w m a) where
    compare (Lazy.WriterT x) (Lazy.WriterT y) = compare1 x y

instance (Read w, Read1 m, Read a) => Read (Lazy.WriterT w m a) where
    readsPrec = readsData $ readsUnary1 "WriterT" Lazy.WriterT

instance (Show w, Show1 m, Show a) => Show (Lazy.WriterT w m a) where
    showsPrec d (Lazy.WriterT m) = showsUnary1 "WriterT" d m

instance (Eq w, Eq1 m) => Eq1 (Lazy.WriterT w m) where eq1 = (==)
instance (Ord w, Ord1 m) => Ord1 (Lazy.WriterT w m) where compare1 = compare
instance (Read w, Read1 m) => Read1 (Lazy.WriterT w m) where readsPrec1 = readsPrec
instance (Show w, Show1 m) => Show1 (Lazy.WriterT w m) where showsPrec1 = showsPrec

instance (Eq w, Eq1 m, Eq a) => Eq (Strict.WriterT w m a) where
    Strict.WriterT x == Strict.WriterT y = eq1 x y

instance (Ord w, Ord1 m, Ord a) => Ord (Strict.WriterT w m a) where
    compare (Strict.WriterT x) (Strict.WriterT y) = compare1 x y

instance (Read w, Read1 m, Read a) => Read (Strict.WriterT w m a) where
    readsPrec = readsData $ readsUnary1 "WriterT" Strict.WriterT

instance (Show w, Show1 m, Show a) => Show (Strict.WriterT w m a) where
    showsPrec d (Strict.WriterT m) = showsUnary1 "WriterT" d m

instance (Eq w, Eq1 m) => Eq1 (Strict.WriterT w m) where eq1 = (==)
instance (Ord w, Ord1 m) => Ord1 (Strict.WriterT w m) where compare1 = compare
instance (Read w, Read1 m) => Read1 (Strict.WriterT w m) where readsPrec1 = readsPrec
instance (Show w, Show1 m) => Show1 (Strict.WriterT w m) where showsPrec1 = showsPrec

instance (Functor f, Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a) where
    Compose x == Compose y = eq1 (fmap Apply x) (fmap Apply y)

instance (Functor f, Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a) where
    compare (Compose x) (Compose y) = compare1 (fmap Apply x) (fmap Apply y)

instance (Functor f, Read1 f, Read1 g, Read a) => Read (Compose f g a) where
    readsPrec = readsData $ readsUnary1 "Compose" (Compose . fmap getApply)

instance (Functor f, Show1 f, Show1 g, Show a) => Show (Compose f g a) where
    showsPrec d (Compose x) = showsUnary1 "Compose" d (fmap Apply x)

instance (Functor f, Eq1 f, Eq1 g) => Eq1 (Compose f g) where eq1 = (==)
instance (Functor f, Ord1 f, Ord1 g) => Ord1 (Compose f g) where
    compare1 = compare
instance (Functor f, Read1 f, Read1 g) => Read1 (Compose f g) where
    readsPrec1 = readsPrec
instance (Functor f, Show1 f, Show1 g) => Show1 (Compose f g) where
    showsPrec1 = showsPrec

instance (Eq1 f, Eq1 g, Eq a) => Eq (Product f g a) where
    Pair x1 y1 == Pair x2 y2 = eq1 x1 x2 && eq1 y1 y2

instance (Ord1 f, Ord1 g, Ord a) => Ord (Product f g a) where
    compare (Pair x1 y1) (Pair x2 y2) =
        compare1 x1 x2 `mappend` compare1 y1 y2

instance (Read1 f, Read1 g, Read a) => Read (Product f g a) where
    readsPrec = readsData $ readsBinary1 "Pair" Pair

instance (Show1 f, Show1 g, Show a) => Show (Product f g a) where
    showsPrec d (Pair x y) = showsBinary1 "Pair" d x y

instance (Eq1 f, Eq1 g) => Eq1 (Product f g) where eq1 = (==)
instance (Ord1 f, Ord1 g) => Ord1 (Product f g) where compare1 = compare
instance (Read1 f, Read1 g) => Read1 (Product f g) where readsPrec1 = readsPrec
instance (Show1 f, Show1 g) => Show1 (Product f g) where showsPrec1 = showsPrec

instance Eq a => Eq1 (Constant a) where eq1 = (==)
instance Ord a => Ord1 (Constant a) where compare1 = compare
instance Read a => Read1 (Constant a) where readsPrec1 = readsPrec
instance Show a => Show1 (Constant a) where showsPrec1 = showsPrec

instance Eq1 Identity where eq1 = (==)
instance Ord1 Identity where compare1 = compare
instance Read1 Identity where readsPrec1 = readsPrec
instance Show1 Identity where showsPrec1 = showsPrec

-- Instances of Prelude classes

-- kludge to get type with the same instances as g a
newtype Apply g a = Apply (g a)

getApply :: Apply g a -> g a
getApply (Apply x) = x

instance (Eq1 g, Eq a) => Eq (Apply g a) where
    Apply x == Apply y = eq1 x y

instance (Ord1 g, Ord a) => Ord (Apply g a) where
    compare (Apply x) (Apply y) = compare1 x y

instance (Read1 g, Read a) => Read (Apply g a) where
    readsPrec d s = [(Apply a, t) | (a, t) <- readsPrec1 d s]

instance (Show1 g, Show a) => Show (Apply g a) where
    showsPrec d (Apply x) = showsPrec1 d x

#if MIN_VERSION_transformers(0,3,0)
instance (Eq1 f, Eq a) => Eq (Lift f a) where
    Pure x1 == Pure x2 = x1 == x2
    Other y1 == Other y2 = eq1 y1 y2
    _ == _ = False

instance (Ord1 f, Ord a) => Ord (Lift f a) where
    compare (Pure x1) (Pure x2) = compare x1 x2
    compare (Pure _) (Other _) = LT
    compare (Other _) (Pure _) = GT
    compare (Other y1) (Other y2) = compare1 y1 y2

instance (Read1 f, Read a) => Read (Lift f a) where
    readsPrec = readsData $
        readsUnary "Pure" Pure `mappend` readsUnary1 "Other" Other

instance (Show1 f, Show a) => Show (Lift f a) where
    showsPrec d (Pure x) = showsUnary "Pure" d x
    showsPrec d (Other y) = showsUnary1 "Other" d y

instance Eq1 f => Eq1 (Lift f) where eq1 = (==)
instance Ord1 f => Ord1 (Lift f) where compare1 = compare
instance Read1 f => Read1 (Lift f) where readsPrec1 = readsPrec
instance Show1 f => Show1 (Lift f) where showsPrec1 = showsPrec

instance (Eq1 f, Eq a) => Eq (Backwards f a) where
    Backwards x == Backwards y = eq1 x y

instance (Ord1 f, Ord a) => Ord (Backwards f a) where
    compare (Backwards x) (Backwards y) = compare1 x y

instance (Read1 f, Read a) => Read (Backwards f a) where
    readsPrec = readsData $ readsUnary1 "Backwards" Backwards

instance (Show1 f, Show a) => Show (Backwards f a) where
    showsPrec d (Backwards x) = showsUnary1 "Backwards" d x

instance Eq1 f => Eq1 (Backwards f) where eq1 = (==)
instance Ord1 f => Ord1 (Backwards f) where compare1 = compare
instance Read1 f => Read1 (Backwards f) where readsPrec1 = readsPrec
instance Show1 f => Show1 (Backwards f) where showsPrec1 = showsPrec

instance (Eq1 f, Eq a) => Eq (Reverse f a) where
    Reverse x == Reverse y = eq1 x y

instance (Ord1 f, Ord a) => Ord (Reverse f a) where
    compare (Reverse x) (Reverse y) = compare1 x y

instance (Read1 f, Read a) => Read (Reverse f a) where
    readsPrec = readsData $ readsUnary1 "Reverse" Reverse

instance (Show1 f, Show a) => Show (Reverse f a) where
    showsPrec d (Reverse x) = showsUnary1 "Reverse" d x

instance (Eq1 f) => Eq1 (Reverse f) where eq1 = (==)
instance (Ord1 f) => Ord1 (Reverse f) where compare1 = compare
instance (Read1 f) => Read1 (Reverse f) where readsPrec1 = readsPrec
instance (Show1 f) => Show1 (Reverse f) where showsPrec1 = showsPrec
#endif
