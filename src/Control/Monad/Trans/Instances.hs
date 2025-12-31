{-# LANGUAGE CPP #-}

#ifndef HASKELL98
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
#endif

{-# OPTIONS_GHC -Wno-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Instances
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Backports orphan instances which are not provided by other modules in
-- @transformers-compat@.
----------------------------------------------------------------------------
module Control.Monad.Trans.Instances () where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(a,b,c) 1
#endif

#ifndef MIN_VERSION_transformers
#define MIN_VERSION_transformers(a,b,c) 1
#endif

import           Control.Applicative.Backwards (Backwards(..))
import           Control.Applicative.Lift (Lift(..))
import qualified Control.Monad.Fail as Fail (MonadFail(..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Accum (AccumT(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Cont (ContT(..))
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.Identity (IdentityT(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Control.Monad.Trans.Select (SelectT(..))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT(..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(..))
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import           Data.Functor.Classes
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Constant (Constant(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Product (Product(..))
import           Data.Functor.Reverse (Reverse(..))
import           Data.Functor.Sum (Sum(..))

import           Control.Applicative
import           Control.Arrow (Arrow((***)))
import           Control.Monad (MonadPlus(..), liftM)
import           Control.Monad.Fix (MonadFix(..))
import           Control.Monad.Zip (MonadZip(..))
import           Data.Bifunctor (Bifunctor(..))
import           Data.Bits
import           Data.Foldable (Foldable(..))
import           Data.Ix (Ix(..))
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Semigroup as Semigroup (Semigroup(..))
import           Data.String (IsString(fromString))
import           Data.Traversable (Traversable(..))
import           Foreign (Storable(..), castPtr)

#if !(MIN_VERSION_transformers(0,6,0))
import           Control.Monad.Trans.Error (Error(..), ErrorT(..))
import           Control.Monad.Trans.List (ListT(..), mapListT)
#endif

#if MIN_VERSION_base(4,10,0)
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bitraversable (Bitraversable(..))
#endif

#ifndef HASKELL98
import           Data.Data (Data)
import           Data.Typeable
import           GHC.Generics
#endif

#if !(MIN_VERSION_transformers(0,5,3))
-- Data.Functor.Reverse
instance (Monad m) => Monad (Reverse m) where
    return a = Reverse (return a)
    {-# INLINE return #-}
    m >>= f = Reverse (getReverse m >>= getReverse . f)
    {-# INLINE (>>=) #-}
    fail msg = Reverse (fail msg)
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (Reverse m) where
    fail msg = Reverse (Fail.fail msg)
    {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (Reverse m) where
    mzero = Reverse mzero
    {-# INLINE mzero #-}
    Reverse x `mplus` Reverse y = Reverse (x `mplus` y)
    {-# INLINE mplus #-}
#endif

#if !(MIN_VERSION_transformers(0,5,4))
# if MIN_VERSION_base(4,10,0)
instance Bifoldable Constant where
    bifoldMap f _ (Constant a) = f a
    {-# INLINE bifoldMap #-}

instance Bitraversable Constant where
    bitraverse f _ (Constant a) = Constant <$> f a
    {-# INLINE bitraverse #-}
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,5))
instance (Semigroup.Semigroup a) => Semigroup.Semigroup (Constant a b) where
    Constant x <> Constant y = Constant (x Semigroup.<> y)
    {-# INLINE (<>) #-}

# if !(MIN_VERSION_transformers(0,6,0))
instance (MonadFix m) => MonadFix (ListT m) where
    mfix f = ListT $ mfix (runListT . f . head) >>= \ xs -> case xs of
        [] -> return []
        x:_ -> liftM (x:) (runListT (mfix (mapListT (liftM tail) . f)))
    {-# INLINE mfix #-}
# endif
#endif

-- Generic(1) instances
#ifndef HASKELL98
# if !(MIN_VERSION_transformers(0,6,0))
-- If we wanted to be 100% faithful to the original Data instance in
-- transformers, we really ought to define an instance like:
--
--   instance (Data a, Typeable k, Typeable (b :: k)) => Data (Constant a b)
--
-- Unfortunately, this is not possible to do with a standalone-derived Data
-- instance (see https://gitlab.haskell.org/ghc/ghc/-/issues/13327).
-- For now, I've opted to just restrict the instance context slightly by using
-- a `Data b` constraint. I'll wait for someone to complain about this before
-- taking further action on it.
deriving instance (Data a, Data b) => Data (Constant a b)

deriving instance Generic  (Constant a b)
deriving instance Generic1 (Constant a)

deriving instance Generic (ContT r m a)

deriving instance Generic  (IdentityT f a)
deriving instance Generic1 (IdentityT f)

deriving instance Generic (MaybeT m a)
deriving instance Functor m => Generic1 (MaybeT m)

deriving instance Generic (Lazy.RWST   r w s m a)
deriving instance Generic (Strict.RWST r w s m a)

deriving instance Generic  (ReaderT r m a)
deriving instance Generic1 (ReaderT r m)

deriving instance Generic (Lazy.StateT   s m a)
deriving instance Generic (Strict.StateT s m a)

deriving instance Generic (Lazy.WriterT   w m a)
deriving instance Generic (Strict.WriterT w m a)

deriving instance Generic  (Backwards f a)
deriving instance Generic1 (Backwards f)

deriving instance Generic  (Lift f a)
deriving instance Generic1 (Lift f)

deriving instance Generic  (Reverse f a)
deriving instance Generic1 (Reverse f)

deriving instance Generic (ExceptT e m a)
deriving instance Functor m => Generic1 (ExceptT e m)

#   if MIN_VERSION_transformers(0,5,3)
deriving instance Generic (AccumT  w m a)
deriving instance Generic (SelectT w m a)
#   endif
# endif
#endif
