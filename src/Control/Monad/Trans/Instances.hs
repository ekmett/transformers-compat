{-# LANGUAGE CPP #-}

#ifndef HASKELL98
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

# if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
# endif

# if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
# endif

# if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DataKinds #-}
# endif

# if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveGeneric #-}
# endif
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
import           Data.Bits
import           Data.Foldable (Foldable(..))
import           Data.Ix (Ix(..))
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..))
import           Data.String (IsString(fromString))
import           Data.Traversable (Traversable(..))
import           Foreign (Storable(..), castPtr)

#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,9,0))
import           Data.Complex (Complex (..))
#endif

#if !(MIN_VERSION_transformers(0,6,0))
import           Control.Monad.Trans.Error (Error(..), ErrorT(..))
import           Control.Monad.Trans.List (ListT(..), mapListT)
#endif

#if MIN_VERSION_base(4,4,0)
import           Control.Monad.Zip (MonadZip(..))
#endif

#if MIN_VERSION_base(4,7,0)
import           Data.Proxy (Proxy(..))
#endif

#if MIN_VERSION_base(4,8,0)
import           Data.Bifunctor (Bifunctor(..))
#endif

#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup (Semigroup(..))
#endif

#if MIN_VERSION_base(4,10,0)
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bitraversable (Bitraversable(..))
#endif

#ifndef HASKELL98
import           Data.Data (Data)
import           Data.Typeable

# ifdef GENERIC_DERIVING
import           Generics.Deriving.Base
# elif __GLASGOW_HASKELL__ >= 702
import           GHC.Generics
# endif
#endif

#if !(MIN_VERSION_transformers(0,3,0))
-- Foldable/Traversable instances
instance (Foldable f) => Foldable (IdentityT f) where
    foldMap f (IdentityT a) = foldMap f a

instance (Traversable f) => Traversable (IdentityT f) where
    traverse f (IdentityT a) = IdentityT <$> traverse f a

instance (Foldable f) => Foldable (MaybeT f) where
    foldMap f (MaybeT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a

instance (Foldable f) => Foldable (Lazy.WriterT w f) where
    foldMap f = foldMap (f . fst) . Lazy.runWriterT

instance (Traversable f) => Traversable (Lazy.WriterT w f) where
    traverse f = fmap Lazy.WriterT . traverse f' . Lazy.runWriterT where
       f' (a, b) = fmap (\ c -> (c, b)) (f a)

instance (Foldable f) => Foldable (Strict.WriterT w f) where
    foldMap f = foldMap (f . fst) . Strict.runWriterT

instance (Traversable f) => Traversable (Strict.WriterT w f) where
    traverse f = fmap Strict.WriterT . traverse f' . Strict.runWriterT where
       f' (a, b) = fmap (\ c -> (c, b)) (f a)

# if !(MIN_VERSION_transformers(0,6,0))
instance (Foldable f) => Foldable (ErrorT e f) where
    foldMap f (ErrorT a) = foldMap (either (const mempty) f) a

instance (Traversable f) => Traversable (ErrorT e f) where
    traverse f (ErrorT a) =
        ErrorT <$> traverse (either (pure . Left) (fmap Right . f)) a

instance (Foldable f) => Foldable (ListT f) where
    foldMap f (ListT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (ListT f) where
    traverse f (ListT a) = ListT <$> traverse (traverse f) a
# endif

-- MonadFix instances for IdentityT and MaybeT
instance (MonadFix m) => MonadFix (IdentityT m) where
    mfix f = IdentityT (mfix (runIdentityT . f))

instance (MonadFix m) => MonadFix (MaybeT m) where
    mfix f = MaybeT (mfix (runMaybeT . f . fromMaybe bomb))
      where bomb = error "mfix (MaybeT): inner computation returned Nothing"

# if !(MIN_VERSION_base(4,9,0))
-- Monad instances for Product
instance (Monad f, Monad g) => Monad (Product f g) where
    return x = Pair (return x) (return x)
    Pair m n >>= f = Pair (m >>= fstP . f) (n >>= sndP . f)
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b

instance (MonadPlus f, MonadPlus g) => MonadPlus (Product f g) where
    mzero = Pair mzero mzero
    Pair x1 y1 `mplus` Pair x2 y2 = Pair (x1 `mplus` x2) (y1 `mplus` y2)

instance (MonadFix f, MonadFix g) => MonadFix (Product f g) where
    mfix f = Pair (mfix (fstP . f)) (mfix (sndP . f))
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b
# endif
#endif

#if !(MIN_VERSION_transformers(0,4,0))
-- Alternative IO instance
# if !(MIN_VERSION_base(4,9,0))
-- The version bounds of transformers prior to 0.4.0.0 should prevent this
-- instance from being compiled on base-4.8.0.0 and later, but we'll put
-- a check here just to be safe.
instance Alternative IO where
    empty = mzero
    (<|>) = mplus
# endif
#endif

#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,4,3))
-- transformers-0.4-specific Eq1, Ord1, Read1, and Show1 instances for Const
instance (Eq a) => Eq1 (Const a) where
    eq1 (Const x) (Const y) = x == y
instance (Ord a) => Ord1 (Const a) where
    compare1 (Const x) (Const y) = compare x y
instance (Read a) => Read1 (Const a) where
    readsPrec1 = readsData $ readsUnary "Const" Const
instance (Show a) => Show1 (Const a) where
    showsPrec1 d (Const x) = showsUnary "Const" d x
#endif

#if !(MIN_VERSION_transformers(0,5,0)) \
  || (MIN_VERSION_transformers(0,5,0) && !(MIN_VERSION_base(4,9,0)))
-- MonadFail instances
instance (Fail.MonadFail m) => Fail.MonadFail (ContT r m) where
    fail msg = ContT $ \ _ -> Fail.fail msg
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (IdentityT m) where
    fail msg = IdentityT $ Fail.fail msg
    {-# INLINE fail #-}

instance (Monad m) => Fail.MonadFail (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (ReaderT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Lazy.RWST r w s m) where
    fail msg = Lazy.RWST $ \ _ _ -> Fail.fail msg
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Strict.RWST r w s m) where
    fail msg = Strict.RWST $ \ _ _ -> Fail.fail msg
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (Lazy.StateT s m) where
    fail str = Lazy.StateT $ \ _ -> Fail.fail str
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (Strict.StateT s m) where
    fail str = Strict.StateT $ \ _ -> Fail.fail str
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Lazy.WriterT w m) where
    fail msg = Lazy.WriterT $ Fail.fail msg
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (Strict.WriterT w m) where
    fail msg = Strict.WriterT $ Fail.fail msg
    {-# INLINE fail #-}

# if !(MIN_VERSION_transformers(0,6,0))
instance (Monad m, Error e) => Fail.MonadFail (ErrorT e m) where
    fail msg = ErrorT $ return (Left (strMsg msg))

instance (Monad m) => Fail.MonadFail (ListT m) where
    fail _ = ListT $ return []
    {-# INLINE fail #-}
# endif

# if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_base(4,9,0))
instance (Fail.MonadFail m) => Fail.MonadFail (ExceptT e m) where
    fail = ExceptT . Fail.fail
    {-# INLINE fail #-}
# endif

# if MIN_VERSION_transformers(0,5,3) && !(MIN_VERSION_base(4,9,0))
instance (Monoid w, Functor m, Fail.MonadFail m) => Fail.MonadFail (AccumT w m) where
    fail msg = AccumT $ const (Fail.fail msg)
    {-# INLINE fail #-}

instance (Fail.MonadFail m) => Fail.MonadFail (SelectT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,0))
-- Monoid Constant instance
instance (Monoid a) => Monoid (Constant a b) where
    mempty = Constant mempty
    Constant x `mappend` Constant y = Constant (x `mappend` y)

-- MonadZip instances
# if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (IdentityT m) where
    mzipWith f (IdentityT a) (IdentityT b) = IdentityT (mzipWith f a b)

instance (MonadZip m) => MonadZip (MaybeT m) where
    mzipWith f (MaybeT a) (MaybeT b) = MaybeT $ mzipWith (liftA2 f) a b

instance (MonadZip m) => MonadZip (ReaderT r m) where
    mzipWith f (ReaderT m) (ReaderT n) = ReaderT $ \ a ->
        mzipWith f (m a) (n a)

instance (Monoid w, MonadZip m) => MonadZip (Lazy.WriterT w m) where
    mzipWith f (Lazy.WriterT x) (Lazy.WriterT y) = Lazy.WriterT $
        mzipWith (\ ~(a, w) ~(b, w') -> (f a b, w `mappend` w')) x y

instance (Monoid w, MonadZip m) => MonadZip (Strict.WriterT w m) where
    mzipWith f (Strict.WriterT x) (Strict.WriterT y) = Strict.WriterT $
        mzipWith (\ (a, w) (b, w') -> (f a b, w `mappend` w')) x y

#  if !(MIN_VERSION_transformers(0,6,0))
instance (MonadZip m) => MonadZip (ListT m) where
    mzipWith f (ListT a) (ListT b) = ListT $ mzipWith (zipWith f) a b
#  endif

#  if !(MIN_VERSION_base(4,8,0))
instance MonadZip Identity where
    mzipWith f (Identity x) (Identity y) = Identity (f x y)
    munzip (Identity (a, b)) = (Identity a, Identity b)
#  endif

#  if !(MIN_VERSION_base(4,9,0))
instance (MonadZip f, MonadZip g) => MonadZip (Product f g) where
    mzipWith f (Pair x1 y1) (Pair x2 y2) = Pair (mzipWith f x1 x2) (mzipWith f y1 y2)
#  endif
# endif

# if MIN_VERSION_base(4,8,0)
-- Bifunctor Constant instance
instance Bifunctor Constant where
    first f (Constant x) = Constant (f x)
    second _ (Constant x) = Constant x
# else
-- Monoid Identity instance
instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)
# endif

# ifndef HASKELL98
-- Typeable instances
#  if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
deriving instance Typeable Backwards
deriving instance Typeable ContT
deriving instance Typeable IdentityT
deriving instance Typeable Lift
deriving instance Typeable MaybeT
deriving instance Typeable MonadTrans
deriving instance Typeable Lazy.RWST
deriving instance Typeable Strict.RWST
deriving instance Typeable ReaderT
deriving instance Typeable Reverse
deriving instance Typeable Lazy.StateT
deriving instance Typeable Strict.StateT
#   if !(MIN_VERSION_transformers(0,6,0))
deriving instance Typeable ErrorT
deriving instance Typeable ListT
#   endif

#   if !(MIN_VERSION_base(4,9,0))
deriving instance Typeable Compose
deriving instance Typeable MonadIO
deriving instance Typeable Product
#   endif
#  endif

-- Identity instances
#  if !(MIN_VERSION_base(4,8,0))
deriving instance Typeable1 Identity
deriving instance Data a => Data (Identity a)
#   if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable 'Identity
#   endif
#  endif

#  if !(MIN_VERSION_base(4,9,0))
#   if __GLASGOW_HASKELL__ >= 708
-- Data instances for Compose and Product
deriving instance (Data (f (g a)), Typeable f, Typeable g, Typeable a)
               => Data (Compose (f :: * -> *) (g :: * -> *) (a :: *))
deriving instance (Data (f a), Data (g a), Typeable f, Typeable g, Typeable a)
               => Data (Product (f :: * -> *) (g :: * -> *) (a :: *))

#    if MIN_VERSION_transformers(0,4,0)
-- Typeable/Data instances for Sum
-- These are also present in Data.Functor.Sum in transformers-compat, but only
-- these are reachable if using @transformers-0.4.0.0@
deriving instance Typeable Sum
deriving instance (Data (f a), Data (g a), Typeable f, Typeable g, Typeable a)
               => Data (Sum (f :: * -> *) (g :: * -> *) (a :: *))
#    endif
#   endif
#  endif
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,1))
# if !(MIN_VERSION_base(4,8,0))
instance (Bounded a) => Bounded (Identity a) where
    minBound = Identity minBound
    maxBound = Identity maxBound

instance (Enum a) => Enum (Identity a) where
    succ (Identity x)     = Identity (succ x)
    pred (Identity x)     = Identity (pred x)
    toEnum i              = Identity (toEnum i)
    fromEnum (Identity x) = fromEnum x
    enumFrom (Identity x) = map Identity (enumFrom x)
    enumFromThen (Identity x) (Identity y) = map Identity (enumFromThen x y)
    enumFromTo   (Identity x) (Identity y) = map Identity (enumFromTo   x y)
    enumFromThenTo (Identity x) (Identity y) (Identity z) =
        map Identity (enumFromThenTo x y z)

instance (Ix a) => Ix (Identity a) where
    range     (Identity x, Identity y) = map Identity (range (x, y))
    index     (Identity x, Identity y) (Identity i) = index     (x, y) i
    inRange   (Identity x, Identity y) (Identity e) = inRange   (x, y) e
    rangeSize (Identity x, Identity y) = rangeSize (x, y)

instance (Storable a) => Storable (Identity a) where
    sizeOf    (Identity x)       = sizeOf x
    alignment (Identity x)       = alignment x
    peekElemOff p i              = fmap Identity (peekElemOff (castPtr p) i)
    pokeElemOff p i (Identity x) = pokeElemOff (castPtr p) i x
    peekByteOff p i              = fmap Identity (peekByteOff p i)
    pokeByteOff p i (Identity x) = pokeByteOff p i x
    peek p                       = fmap runIdentity (peek (castPtr p))
    poke p (Identity x)          = poke (castPtr p) x
# endif
#endif

#if !(MIN_VERSION_transformers(0,5,3))
# if !(MIN_VERSION_base(4,9,0))
#  if MIN_VERSION_base(4,7,0)
-- Data.Proxy
#   if defined(TRANSFORMERS_FOUR)
instance Eq1 Proxy where
    eq1 _ _ = True

instance Ord1 Proxy where
    compare1 _ _ = EQ

instance Show1 Proxy where
    showsPrec1 _ _ = showString "Proxy"

instance Read1 Proxy where
    readsPrec1 d =
        readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])
#   elif MIN_VERSION_transformers(0,5,0)
instance Eq1 Proxy where
    liftEq _ _ _ = True

instance Ord1 Proxy where
    liftCompare _ _ _ = EQ

instance Show1 Proxy where
    liftShowsPrec _ _ _ _ = showString "Proxy"

instance Read1 Proxy where
    liftReadsPrec _ _ d =
        readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])
#   endif
#  endif
# endif

# if !(MIN_VERSION_base(4,8,0))
-- Data.Functor.Identity
instance (Bits a) => Bits (Identity a) where
    Identity x .&. Identity y     = Identity (x .&. y)
    Identity x .|. Identity y     = Identity (x .|. y)
    xor (Identity x) (Identity y) = Identity (xor x y)
    complement   (Identity x)     = Identity (complement x)
    shift        (Identity x) i   = Identity (shift    x i)
    rotate       (Identity x) i   = Identity (rotate   x i)
    setBit       (Identity x) i   = Identity (setBit   x i)
    clearBit     (Identity x) i   = Identity (clearBit x i)
    shiftL       (Identity x) i   = Identity (shiftL   x i)
    shiftR       (Identity x) i   = Identity (shiftR   x i)
    rotateL      (Identity x) i   = Identity (rotateL  x i)
    rotateR      (Identity x) i   = Identity (rotateR  x i)
    testBit      (Identity x) i   = testBit x i
    bitSize      (Identity x)     = bitSize x
    isSigned     (Identity x)     = isSigned x
    bit i                         = Identity (bit i)
#  if MIN_VERSION_base(4,5,0)
    unsafeShiftL (Identity x) i   = Identity (unsafeShiftL x i)
    unsafeShiftR (Identity x) i   = Identity (unsafeShiftR x i)
    popCount     (Identity x)     = popCount x
#  endif
#  if MIN_VERSION_base(4,7,0)
    zeroBits                      = Identity zeroBits
    bitSizeMaybe (Identity x)     = bitSizeMaybe x
#  endif

#  if MIN_VERSION_base(4,7,0)
instance (FiniteBits a) => FiniteBits (Identity a) where
    finiteBitSize (Identity x) = finiteBitSize x
#  endif

instance (Floating a) => Floating (Identity a) where
    pi                                = Identity pi
    exp   (Identity x)                = Identity (exp x)
    log   (Identity x)                = Identity (log x)
    sqrt  (Identity x)                = Identity (sqrt x)
    sin   (Identity x)                = Identity (sin x)
    cos   (Identity x)                = Identity (cos x)
    tan   (Identity x)                = Identity (tan x)
    asin  (Identity x)                = Identity (asin x)
    acos  (Identity x)                = Identity (acos x)
    atan  (Identity x)                = Identity (atan x)
    sinh  (Identity x)                = Identity (sinh x)
    cosh  (Identity x)                = Identity (cosh x)
    tanh  (Identity x)                = Identity (tanh x)
    asinh (Identity x)                = Identity (asinh x)
    acosh (Identity x)                = Identity (acosh x)
    atanh (Identity x)                = Identity (atanh x)
    Identity x ** Identity y          = Identity (x ** y)
    logBase (Identity x) (Identity y) = Identity (logBase x y)

instance (Fractional a) => Fractional (Identity a) where
    Identity x / Identity y = Identity (x / y)
    recip (Identity x)      = Identity (recip x)
    fromRational r          = Identity (fromRational r)

instance (IsString a) => IsString (Identity a) where
    fromString s = Identity (fromString s)

instance (Integral a) => Integral (Identity a) where
    quot    (Identity x) (Identity y) = Identity (quot x y)
    rem     (Identity x) (Identity y) = Identity (rem  x y)
    div     (Identity x) (Identity y) = Identity (div  x y)
    mod     (Identity x) (Identity y) = Identity (mod  x y)
    quotRem (Identity x) (Identity y) = (Identity *** Identity) (quotRem x y)
    divMod  (Identity x) (Identity y) = (Identity *** Identity) (divMod  x y)
    toInteger (Identity x)            = toInteger x

instance (Num a) => Num (Identity a) where
    Identity x + Identity y = Identity (x + y)
    Identity x - Identity y = Identity (x - y)
    Identity x * Identity y = Identity (x * y)
    negate (Identity x)     = Identity (negate x)
    abs    (Identity x)     = Identity (abs    x)
    signum (Identity x)     = Identity (signum x)
    fromInteger n           = Identity (fromInteger n)

instance (Real a) => Real (Identity a) where
    toRational (Identity x) = toRational x

instance (RealFloat a) => RealFloat (Identity a) where
    floatRadix     (Identity x)     = floatRadix     x
    floatDigits    (Identity x)     = floatDigits    x
    floatRange     (Identity x)     = floatRange     x
    decodeFloat    (Identity x)     = decodeFloat    x
    exponent       (Identity x)     = exponent       x
    isNaN          (Identity x)     = isNaN          x
    isInfinite     (Identity x)     = isInfinite     x
    isDenormalized (Identity x)     = isDenormalized x
    isNegativeZero (Identity x)     = isNegativeZero x
    isIEEE         (Identity x)     = isIEEE         x
    significand    (Identity x)     = significand (Identity x)
    scaleFloat s   (Identity x)     = Identity (scaleFloat s x)
    encodeFloat m n                 = Identity (encodeFloat m n)
    atan2 (Identity x) (Identity y) = Identity (atan2 x y)

instance (RealFrac a) => RealFrac (Identity a) where
    properFraction (Identity x) = (id *** Identity) (properFraction x)
    truncate       (Identity x) = truncate x
    round          (Identity x) = round    x
    ceiling        (Identity x) = ceiling  x
    floor          (Identity x) = floor    x
# endif

# if MIN_VERSION_transformers(0,3,0)
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
# endif
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
# if MIN_VERSION_base(4,9,0)
instance (Semigroup.Semigroup a) => Semigroup.Semigroup (Constant a b) where
    Constant x <> Constant y = Constant (x Semigroup.<> y)
    {-# INLINE (<>) #-}
# endif

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
# if (!(MIN_VERSION_transformers(0,5,0)) && (__GLASGOW_HASKELL__ >= 702 || defined(GENERIC_DERIVING))) \
    || (MIN_VERSION_transformers(0,5,0)  &&  __GLASGOW_HASKELL__ < 702  && defined(GENERIC_DERIVING))

#  if !(MIN_VERSION_base(4,8,0))
instance Generic (Identity a) where
    type Rep (Identity a) = D1 MDIdentity (C1 MCIdentity (S1 MSIdentity (Rec0 a)))
    from (Identity x) = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = Identity x

instance Generic1 Identity where
    type Rep1 Identity = D1 MDIdentity (C1 MCIdentity (S1 MSIdentity Par1))
    from1 (Identity x) = M1 (M1 (M1 (Par1 x)))
    to1 (M1 (M1 (M1 x))) = Identity (unPar1 x)

data MDIdentity
data MCIdentity
data MSIdentity

instance Datatype MDIdentity where
  datatypeName _ = "Identity"
  moduleName _ = "Data.Functor.Identity"
#   if __GLASGOW_HASKELL__ >= 708
  isNewtype _ = True
#   endif

instance Constructor MCIdentity where
  conName _ = "Identity"
  conIsRecord _ = True

instance Selector MSIdentity where
  selName _ = "runIdentity"
#  endif

#  if !(MIN_VERSION_base(4,9,0))
-- Generic(1) instances for Compose
instance Generic (Compose f g a) where
    type Rep (Compose f g a) =
      D1 MDCompose
        (C1 MCCompose
          (S1 MSCompose (Rec0 (f (g a)))))
    from (Compose x) = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = Compose x

instance Functor f => Generic1 (Compose f g) where
    type Rep1 (Compose f g) =
      D1 MDCompose
        (C1 MCCompose
          (S1 MSCompose (f :.: Rec1 g)))
    from1 (Compose x) = M1 (M1 (M1 (Comp1 (fmap Rec1 x))))
    to1 (M1 (M1 (M1 x))) = Compose (fmap unRec1 (unComp1 x))

data MDCompose
data MCCompose
data MSCompose

instance Datatype MDCompose where
    datatypeName _ = "Compose"
    moduleName   _ = "Data.Functor.Compose"
#   if __GLASGOW_HASKELL__ >= 708
    isNewtype    _ = True
#   endif

instance Constructor MCCompose where
    conName     _ = "Compose"
    conIsRecord _ = True

instance Selector MSCompose where
    selName _ = "getCompose"

-- Generic(1) instances for Product
instance Generic (Product f g a) where
    type Rep (Product f g a) =
      D1 MDProduct
        (C1 MCPair
          (S1 NoSelector (Rec0 (f a)) :*: S1 NoSelector (Rec0 (g a))))
    from (Pair f g) = M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))
    to (M1 (M1 (M1 (K1 f) :*: M1 (K1 g)))) = Pair f g

instance Generic1 (Product f g) where
    type Rep1 (Product f g) =
      D1 MDProduct
        (C1 MCPair
          (S1 NoSelector (Rec1 f) :*: S1 NoSelector (Rec1 g)))
    from1 (Pair f g) = M1 (M1 (M1 (Rec1 f) :*: M1 (Rec1 g)))
    to1 (M1 (M1 (M1 f :*: M1 g))) = Pair (unRec1 f) (unRec1 g)

data MDProduct
data MCPair

instance Datatype MDProduct where
    datatypeName _ = "Product"
    moduleName   _ = "Data.Functor.Product"

instance Constructor MCPair where
    conName _ = "Pair"

#   if MIN_VERSION_transformers(0,4,0)
-- Generic(1) instances for Sum
-- These are also present in Data.Functor.Sum in transformers-compat, but only
-- these are reachable if using @transformers-0.4.0.0@ or later
instance Generic (Sum f g a) where
    type Rep (Sum f g a) =
      D1 MDSum (C1 MCInL (S1 NoSelector (Rec0 (f a)))
            :+: C1 MCInR (S1 NoSelector (Rec0 (g a))))
    from (InL f) = M1 (L1 (M1 (M1 (K1 f))))
    from (InR g) = M1 (R1 (M1 (M1 (K1 g))))
    to (M1 (L1 (M1 (M1 (K1 f))))) = InL f
    to (M1 (R1 (M1 (M1 (K1 g))))) = InR g

instance Generic1 (Sum f g) where
    type Rep1 (Sum f g) =
      D1 MDSum (C1 MCInL (S1 NoSelector (Rec1 f))
            :+: C1 MCInR (S1 NoSelector (Rec1 g)))
    from1 (InL f) = M1 (L1 (M1 (M1 (Rec1 f))))
    from1 (InR g) = M1 (R1 (M1 (M1 (Rec1 g))))
    to1 (M1 (L1 (M1 (M1 f)))) = InL (unRec1 f)
    to1 (M1 (R1 (M1 (M1 g)))) = InR (unRec1 g)

data MDSum
data MCInL
data MCInR

instance Datatype MDSum where
    datatypeName _ = "Sum"
    moduleName   _ = "Data.Functor.Sum"

instance Constructor MCInL where
    conName _ = "InL"

instance Constructor MCInR where
    conName _ = "InR"
#   endif
#  endif

# endif

# if !(MIN_VERSION_transformers(0,6,0))
#  if __GLASGOW_HASKELL__ >= 708
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
#   if __GLASGOW_HASKELL__ < 710
deriving instance Typeable Constant
#   endif
#  endif

-- The use of GHC 8.0 in this CPP is a conservative lower bound for
-- determining the earliest version of GHC that can derive Generic(1)
-- instances for all data types without bugs. We might be able to pick
-- an earlier GHC version for certain data types, but it doesn't seem
-- worthwhile, given that we'll have to fall back on hand-written instances at
-- some point anyway.
#  if __GLASGOW_HASKELL__ >= 800

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

#   if MIN_VERSION_transformers(0,3,0)
deriving instance Generic  (Backwards f a)
deriving instance Generic1 (Backwards f)

deriving instance Generic  (Lift f a)
deriving instance Generic1 (Lift f)

deriving instance Generic  (Reverse f a)
deriving instance Generic1 (Reverse f)
#   endif

#   if MIN_VERSION_transformers(0,4,0)
deriving instance Generic (ExceptT e m a)
deriving instance Functor m => Generic1 (ExceptT e m)
#   endif

#   if MIN_VERSION_transformers(0,5,3)
deriving instance Generic (AccumT  w m a)
deriving instance Generic (SelectT w m a)
#   endif

#  elif __GLASGOW_HASKELL__ >= 702 || defined(GENERIC_DERIVING)

instance Generic (Constant a b) where
  type Rep (Constant a b) = D1 D1'Constant (C1 C1_0'Constant (S1 S1_0_0'Constant (Rec0 a)))
  from (Constant x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Constant x

instance Generic1 (Constant a) where
  type Rep1 (Constant a) = D1 D1'Constant (C1 C1_0'Constant (S1 S1_0_0'Constant (Rec0 a)))
  from1 (Constant x) = M1 (M1 (M1 (K1 x)))
  to1 (M1 (M1 (M1 x))) = Constant (unK1 x)

instance Datatype D1'Constant where
  datatypeName _ = "Constant"
  moduleName _ = "Data.Functor.Constant"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'Constant where
  conName _ = "Constant"
  conIsRecord _ = True

instance Selector S1_0_0'Constant where
  selName _ = "getConstant"

data D1'Constant
data C1_0'Constant
data S1_0_0'Constant

-----

instance Generic (ContT r m a) where
  type Rep (ContT r m a) = D1 D1'ContT (C1 C1_0'ContT (S1 S1_0_0'ContT (Rec0 ((a -> m r) -> m r))))
  from (ContT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = ContT x

instance Datatype D1'ContT where
  datatypeName _ = "ContT"
  moduleName _ = "Control.Monad.Trans.Cont"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'ContT where
  conName _ = "ContT"
  conIsRecord _ = True

instance Selector S1_0_0'ContT where
  selName _ = "runContT"

data D1'ContT
data C1_0'ContT
data S1_0_0'ContT

-----

instance Generic (IdentityT f a) where
  type Rep (IdentityT f a) = D1 D1'IdentityT (C1 C1_0'IdentityT (S1 S1_0_0'IdentityT (Rec0 (f a))))
  from (IdentityT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = IdentityT x

instance Generic1 (IdentityT f) where
  type Rep1 (IdentityT f) = D1 D1'IdentityT (C1 C1_0'IdentityT (S1 S1_0_0'IdentityT (Rec1 f)))
  from1 (IdentityT x) = M1 (M1 (M1 (Rec1 x)))
  to1 (M1 (M1 (M1 x))) = IdentityT (unRec1 x)

instance Datatype D1'IdentityT where
  datatypeName _ = "IdentityT"
  moduleName _ = "Control.Monad.Trans.Identity"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'IdentityT where
  conName _ = "IdentityT"
  conIsRecord _ = True

instance Selector S1_0_0'IdentityT where
  selName _ = "runIdentityT"

data D1'IdentityT
data C1_0'IdentityT
data S1_0_0'IdentityT

-----

instance Generic (MaybeT m a) where
  type Rep (MaybeT m a) = D1 D1'MaybeT (C1 C1_0'MaybeT (S1 S1_0_0'MaybeT (Rec0 (m (Maybe a)))))
  from (MaybeT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = MaybeT x

instance Functor m => Generic1 (MaybeT m) where
  type Rep1 (MaybeT m) = D1 D1'MaybeT (C1 C1_0'MaybeT (S1 S1_0_0'MaybeT (m :.: Rec1 Maybe)))
  from1 (MaybeT x) = M1 (M1 (M1 ((.) Comp1 (fmap Rec1) x)))
  to1 (M1 (M1 (M1 x))) = MaybeT ((.) (fmap unRec1) unComp1 x)

instance Datatype D1'MaybeT where
  datatypeName _ = "MaybeT"
  moduleName _ = "Control.Monad.Trans.Maybe"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'MaybeT where
  conName _ = "MaybeT"
  conIsRecord _ = True

instance Selector S1_0_0'MaybeT where
  selName _ = "runMaybeT"

data D1'MaybeT
data C1_0'MaybeT
data S1_0_0'MaybeT

-----

instance Generic (Lazy.RWST r w s m a) where
  type Rep (Lazy.RWST r w s m a) = D1 D1'RWSTLazy (C1 C1_0'RWST (S1 S1_0_0'RWST (Rec0 (r -> s -> m (a, s, w)))))
  from (Lazy.RWST x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Lazy.RWST x

instance Generic (Strict.RWST r w s m a) where
  type Rep (Strict.RWST r w s m a) = D1 D1'RWSTStrict (C1 C1_0'RWST (S1 S1_0_0'RWST (Rec0 (r -> s -> m (a, s, w)))))
  from (Strict.RWST x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Strict.RWST x

instance Datatype D1'RWSTLazy where
  datatypeName _ = "RWST"
  moduleName _ = "Control.Monad.Trans.RWS.Lazy"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Datatype D1'RWSTStrict where
  datatypeName _ = "RWST"
  moduleName _ = "Control.Monad.Trans.RWS.Strict"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'RWST where
  conName _ = "RWST"
  conIsRecord _ = True

instance Selector S1_0_0'RWST where
  selName _ = "runRWST"

data D1'RWSTLazy
data D1'RWSTStrict
data C1_0'RWST
data S1_0_0'RWST

-----

instance Generic (ReaderT r m a) where
  type Rep (ReaderT r m a) = D1 D1'ReaderT (C1 C1_0'ReaderT (S1 S1_0_0'ReaderT (Rec0 (r -> m a))))
  from (ReaderT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = ReaderT x

instance Datatype D1'ReaderT where
  datatypeName _ = "ReaderT"
  moduleName _ = "Control.Monad.Trans.Reader"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'ReaderT where
  conName _ = "ReaderT"
  conIsRecord _ = True

instance Selector S1_0_0'ReaderT where
  selName _ = "runReaderT"

data D1'ReaderT
data C1_0'ReaderT
data S1_0_0'ReaderT

-----

instance Generic (Lazy.StateT s m a) where
  type Rep (Lazy.StateT s m a) = D1 D1'StateTLazy (C1 C1_0'StateT (S1 S1_0_0'StateT (Rec0 (s -> m (a, s)))))
  from (Lazy.StateT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Lazy.StateT x

instance Generic (Strict.StateT s m a) where
  type Rep (Strict.StateT s m a) = D1 D1'StateTStrict (C1 C1_0'StateT (S1 S1_0_0'StateT (Rec0 (s -> m (a, s)))))
  from (Strict.StateT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Strict.StateT x

instance Datatype D1'StateTLazy where
  datatypeName _ = "StateT"
  moduleName _ = "Control.Monad.Trans.State.Lazy"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Datatype D1'StateTStrict where
  datatypeName _ = "StateT"
  moduleName _ = "Control.Monad.Trans.State.Strict"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'StateT where
  conName _ = "StateT"
  conIsRecord _ = True

instance Selector S1_0_0'StateT where
  selName _ = "runStateT"

data D1'StateTLazy
data D1'StateTStrict
data C1_0'StateT
data S1_0_0'StateT

-----

instance Generic (Lazy.WriterT w m a) where
  type Rep (Lazy.WriterT w m a) = D1 D1'WriterTLazy (C1 C1_0'WriterT (S1 S1_0_0'WriterT (Rec0 (m (a, w)))))
  from (Lazy.WriterT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Lazy.WriterT x

instance Generic (Strict.WriterT w m a) where
  type Rep (Strict.WriterT w m a) = D1 D1'WriterTStrict (C1 C1_0'WriterT (S1 S1_0_0'WriterT (Rec0 (m (a, w)))))
  from (Strict.WriterT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Strict.WriterT x

instance Datatype D1'WriterTLazy where
  datatypeName _ = "WriterT"
  moduleName _ = "Control.Monad.Trans.Writer.Lazy"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Datatype D1'WriterTStrict where
  datatypeName _ = "WriterT"
  moduleName _ = "Control.Monad.Trans.Writer.Strict"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'WriterT where
  conName _ = "WriterT"
  conIsRecord _ = True

instance Selector S1_0_0'WriterT where
  selName _ = "runWriterT"

data D1'WriterTLazy
data D1'WriterTStrict
data C1_0'WriterT
data S1_0_0'WriterT

#   if MIN_VERSION_transformers(0,3,0)
instance Generic (Backwards f a) where
  type Rep (Backwards f a) = D1 D1'Backwards (C1 C1_0'Backwards (S1 S1_0_0'Backwards (Rec0 (f a))))
  from (Backwards x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Backwards x

instance Generic1 (Backwards f) where
  type Rep1 (Backwards f) = D1 D1'Backwards (C1 C1_0'Backwards (S1 S1_0_0'Backwards (Rec1 f)))
  from1 (Backwards x) = M1 (M1 (M1 (Rec1 x)))
  to1 (M1 (M1 (M1 x))) = Backwards (unRec1 x)

instance Datatype D1'Backwards where
  datatypeName _ = "Backwards"
  moduleName _ = "Control.Applicative.Backwards"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'Backwards where
  conName _ = "Backwards"
  conIsRecord _ = True

instance Selector S1_0_0'Backwards where
  selName _ = "forwards"

data D1'Backwards
data C1_0'Backwards
data S1_0_0'Backwards

-----

instance Generic (Reverse f a) where
  type Rep (Reverse f a) = D1 D1'Reverse (C1 C1_0'Reverse (S1 S1_0_0'Reverse (Rec0 (f a))))
  from (Reverse x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = Reverse x

instance Generic1 (Reverse f) where
  type Rep1 (Reverse f) = D1 D1'Reverse (C1 C1_0'Reverse (S1 S1_0_0'Reverse (Rec1 f)))
  from1 (Reverse x) = M1 (M1 (M1 (Rec1 x)))
  to1 (M1 (M1 (M1 x))) = Reverse (unRec1 x)

instance Datatype D1'Reverse where
  datatypeName _ = "Reverse"
  moduleName _ = "Data.Functor.Reverse"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'Reverse where
  conName _ = "Reverse"
  conIsRecord _ = True

instance Selector S1_0_0'Reverse where
  selName _ = "getReverse"

data D1'Reverse
data C1_0'Reverse
data S1_0_0'Reverse

-----

instance Generic (Lift f a) where
  type Rep (Lift f a) = D1 D1'Lift (C1 C1_0'Lift (S1 NoSelector (Rec0 a)) :+: C1 C1_1'Lift (S1 NoSelector (Rec0 (f a))))
  from (Pure x) = M1 (L1 (M1 (M1 (K1 x))))
  from (Other x) = M1 (R1 (M1 (M1 (K1 x))))
  to (M1 (L1 (M1 (M1 (K1 x))))) = Pure x
  to (M1 (R1 (M1 (M1 (K1 x))))) = Other x

instance Generic1 (Lift f) where
  type Rep1 (Lift f) = D1 D1'Lift (C1 C1_0'Lift (S1 NoSelector Par1) :+: C1 C1_1'Lift (S1 NoSelector (Rec1 f)))
  from1 (Pure x) = M1 (L1 (M1 (M1 (Par1 x))))
  from1 (Other x) = M1 (R1 (M1 (M1 (Rec1 x))))
  to1 (M1 (L1 (M1 (M1 x)))) = Pure (unPar1 x)
  to1 (M1 (R1 (M1 (M1 x)))) = Other (unRec1 x)

instance Datatype D1'Lift where
  datatypeName _ = "Lift"
  moduleName _ = "Control.Applicative.Lift"

instance Constructor C1_0'Lift where
  conName _ = "Pure"

instance Constructor C1_1'Lift where
  conName _ = "Other"

data D1'Lift
data C1_0'Lift
data C1_1'Lift
#   endif

#   if MIN_VERSION_transformers(0,4,0)
instance Generic (ExceptT e m a) where
  type Rep (ExceptT e m a) = D1 D1'ExceptT (C1 C1_0'ExceptT (S1 NoSelector (Rec0 (m (Either e a)))))
  from (ExceptT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = ExceptT x

instance Functor m => Generic1 (ExceptT e m) where
  type Rep1 (ExceptT e m) = D1 D1'ExceptT (C1 C1_0'ExceptT (S1 NoSelector (m :.: Rec1 (Either e))))
  from1 (ExceptT x) = M1 (M1 (M1 ((.) Comp1 (fmap Rec1) x)))
  to1 (M1 (M1 (M1 x))) = ExceptT ((.) (fmap unRec1) unComp1 x)

instance Datatype D1'ExceptT where
  datatypeName _ = "ExceptT"
  moduleName _ = "Control.Monad.Trans.Except"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'ExceptT where
  conName _ = "ExceptT"

data D1'ExceptT
data C1_0'ExceptT
#   endif

#   if MIN_VERSION_transformers(0,5,3)
instance Generic (AccumT w m a) where
  type Rep (AccumT w m a) = D1 D1'AccumT (C1 C1_0'AccumT (S1 NoSelector (Rec0 (w -> m (a, w)))))
  from (AccumT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = AccumT x

instance Datatype D1'AccumT where
  datatypeName _ = "AccumT"
  moduleName _ = "Control.Monad.Trans.Accum"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'AccumT where
  conName _ = "AccumT"

data D1'AccumT
data C1_0'AccumT

-----

instance Generic (SelectT r m a) where
  type Rep (SelectT r m a) = D1 D1'SelectT (C1 C1_0'SelectT (S1 NoSelector (Rec0 ((a -> m r) -> m a))))
  from (SelectT x) = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = SelectT x

instance Datatype D1'SelectT where
  datatypeName _ = "SelectT"
  moduleName _ = "Control.Monad.Trans.Select"
#    if MIN_VERSION_base(4,7,0)
  isNewtype _ = True
#    endif

instance Constructor C1_0'SelectT where
  conName _ = "SelectT"

data D1'SelectT
data C1_0'SelectT
#   endif

#  endif
# endif
#endif

#if !(MIN_VERSION_base(4,9,0))
# if defined(TRANSFORMERS_FOUR)

#  if MIN_VERSION_base(4,4,0)
instance Eq1 Complex where eq1 = (==)
instance Read1 Complex where readsPrec1 = readsPrec
instance Show1 Complex where showsPrec1 = showsPrec
#  endif

instance (Eq a, Eq b) => Eq1 ((,,) a b) where eq1 = (==)
instance (Ord a, Ord b) => Ord1 ((,,) a b) where compare1 = compare
instance (Read a, Read b) => Read1 ((,,) a b) where readsPrec1 = readsPrec
instance (Show a, Show b) => Show1 ((,,) a b) where showsPrec1 = showsPrec

instance (Eq a, Eq b, Eq c) => Eq1 ((,,,) a b c) where eq1 = (==)
instance (Ord a, Ord b, Ord c) => Ord1 ((,,,) a b c) where compare1 = compare
instance (Read a, Read b, Read c) => Read1 ((,,,) a b c) where readsPrec1 = readsPrec
instance (Show a, Show b, Show c) => Show1 ((,,,) a b c) where showsPrec1 = showsPrec
# elif MIN_VERSION_transformers(0,5,0)

#  if MIN_VERSION_base(4,4,0)
instance Eq1 Complex where
    liftEq eq (x :+ y) (u :+ v) = eq x u && eq y v

instance Read1 Complex where
    liftReadsPrec rdP _ p s = readParen (p > complexPrec) (\s' -> do
      (x, s'')     <- rdP (complexPrec+1) s'
      (":+", s''') <- lex s''
      (y, s'''')   <- rdP (complexPrec+1) s'''
      return (x :+ y, s'''')) s
      where
        complexPrec = 6

instance Show1 Complex where
    liftShowsPrec sp _ d (x :+ y) = showParen (d > complexPrec) $
        sp (complexPrec+1) x . showString " :+ " . sp (complexPrec+1) y
      where
        complexPrec = 6
#  endif

instance Eq a => Eq2 ((,,) a) where
    liftEq2 e1 e2 (u1, x1, y1) (v1, x2, y2) =
        u1 == v1 &&
        e1 x1 x2 && e2 y1 y2

instance Ord a => Ord2 ((,,) a) where
    liftCompare2 comp1 comp2 (u1, x1, y1) (v1, x2, y2) =
        compare u1 v1 `mappend`
        comp1 x1 x2 `mappend` comp2 y1 y2

instance Read a => Read2 ((,,) a) where
    liftReadsPrec2 rp1 _ rp2 _ _ = readParen False $ \ r ->
        [((e1,e2,e3), y) | ("(",s) <- lex r,
                           (e1,t)  <- readsPrec 0 s,
                           (",",u) <- lex t,
                           (e2,v)  <- rp1 0 u,
                           (",",w) <- lex v,
                           (e3,x)  <- rp2 0 w,
                           (")",y) <- lex x]

instance Show a => Show2 ((,,) a) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x1,y1,y2)
        = showChar '(' . showsPrec 0 x1
        . showChar ',' . sp1 0 y1
        . showChar ',' . sp2 0 y2
        . showChar ')'

instance (Eq a, Eq b) => Eq1 ((,,) a b) where
    liftEq = liftEq2 (==)

instance (Ord a, Ord b) => Ord1 ((,,) a b) where
    liftCompare = liftCompare2 compare

instance (Read a, Read b) => Read1 ((,,) a b) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Show a, Show b) => Show1 ((,,) a b) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Eq a, Eq b) => Eq2 ((,,,) a b) where
    liftEq2 e1 e2 (u1, u2, x1, y1) (v1, v2, x2, y2) =
        u1 == v1 &&
        u2 == v2 &&
        e1 x1 x2 && e2 y1 y2

instance (Ord a, Ord b) => Ord2 ((,,,) a b) where
    liftCompare2 comp1 comp2 (u1, u2, x1, y1) (v1, v2, x2, y2) =
        compare u1 v1 `mappend`
        compare u2 v2 `mappend`
        comp1 x1 x2 `mappend` comp2 y1 y2

instance (Read a, Read b) => Read2 ((,,,) a b) where
    liftReadsPrec2 rp1 _ rp2 _ _ = readParen False $ \ r ->
        [((e1,e2,e3,e4), s9) | ("(",s1) <- lex r,
                               (e1,s2)  <- readsPrec 0 s1,
                               (",",s3) <- lex s2,
                               (e2,s4)  <- readsPrec 0 s3,
                               (",",s5) <- lex s4,
                               (e3,s6)  <- rp1 0 s5,
                               (",",s7) <- lex s6,
                               (e4,s8)  <- rp2 0 s7,
                               (")",s9) <- lex s8]

instance (Show a, Show b) => Show2 ((,,,) a b) where
    liftShowsPrec2 sp1 _ sp2 _ _ (x1,x2,y1,y2)
        = showChar '(' . showsPrec 0 x1
        . showChar ',' . showsPrec 0 x2
        . showChar ',' . sp1 0 y1
        . showChar ',' . sp2 0 y2
        . showChar ')'

instance (Eq a, Eq b, Eq c) => Eq1 ((,,,) a b c) where
    liftEq = liftEq2 (==)

instance (Ord a, Ord b, Ord c) => Ord1 ((,,,) a b c) where
    liftCompare = liftCompare2 compare

instance (Read a, Read b, Read c) => Read1 ((,,,) a b c) where
    liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Show a, Show b, Show c) => Show1 ((,,,) a b c) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

# endif
#endif
