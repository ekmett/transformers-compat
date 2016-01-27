{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Functor.Classes.Generic
  ( -- * 'Eq1'
    liftEqDefault
    -- * 'Ord1'
  , liftCompareDefault
    -- * 'Show1'
  , liftShowsPrecDefault
  ) where

import Data.Functor.Classes
import GHC.Generics
import GHC.Show (appPrec, appPrec1, showSpace)
import Text.Show (showListWith)

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

#if MIN_VERSION_base(4,9,0)
import GHC.Exts
#endif

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
  gliftEq _ _ _ = False

instance GEq1 f => GEq1 (M1 i c f) where
  gliftEq f (M1 a) (M1 b) = gliftEq f a b

instance GEq1 U1 where
  gliftEq _ U1 U1 = True

instance GEq1 V1 where
  gliftEq _ !_ = undefined

instance GEq1 Par1 where
  gliftEq f (Par1 a) (Par1 b) = f a b

#if MIN_VERSION_base(4,9,0)
-- Unboxed types
instance GEq1 UAddr where
  gliftEq _ (UAddr a1) (UAddr a2) = isTrue# (eqAddr# a1 a2)

instance GEq1 UChar where
  gliftEq _ (UChar c1) (UChar c2) = isTrue# (eqChar# c1 c2)

instance GEq1 UDouble where
  gliftEq _ (UDouble d1) (UDouble d2) = isTrue# (d1 ==## d2)

instance GEq1 UFloat where
  gliftEq _ (UFloat f1) (UFloat f2) = isTrue# (eqFloat# f1 f2)

instance GEq1 UInt where
  gliftEq _ (UInt i1) (UInt i2) = isTrue# (i1 ==# i2)

instance GEq1 UWord where
  gliftEq _ (UWord w1) (UWord w2) = isTrue# (eqWord# w1 w2)
#endif

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
  gliftCompare _ L1{} R1{} = LT
  gliftCompare _ R1{} L1{} = GT
  gliftCompare f (R1 b) (R1 d) = gliftCompare f b d

instance GOrd1 f => GOrd1 (M1 i c f) where
  gliftCompare f (M1 a) (M1 b) = gliftCompare f a b

instance GOrd1 U1 where
  gliftCompare _ U1 U1 = EQ

instance GOrd1 V1 where
  gliftCompare _ !_ = undefined

instance GOrd1 Par1 where
  gliftCompare f (Par1 a) (Par1 b) = f a b

#if MIN_VERSION_base(4,9,0)
-- Unboxed types
instance GOrd1 UAddr where
  gliftCompare _ (UAddr a1) (UAddr a2) = primCompare (eqAddr# a1 a2) (leAddr# a1 a2)

instance GOrd1 UChar where
  gliftCompare _ (UChar c1) (UChar c2) = primCompare (eqChar# c1 c2) (leChar# c1 c2)

instance GOrd1 UDouble where
  gliftCompare _ (UDouble d1) (UDouble d2) = primCompare (d1 ==## d2) (d1 <=## d2)

instance GOrd1 UFloat where
  gliftCompare _ (UFloat f1) (UFloat f2) = primCompare (eqFloat# f1 f2) (leFloat# f1 f2)

instance GOrd1 UInt where
  gliftCompare _ (UInt i1) (UInt i2) = primCompare (i1 ==# i2) (i1 <=# i2)

instance GOrd1 UWord where
  gliftCompare _ (UWord w1) (UWord w2) = primCompare (eqWord# w1 w2) (leWord# w1 w2)

primCompare :: Int# -> Int# -> Ordering
primCompare eq le = if isTrue# eq then EQ
                    else if isTrue# le then LT
                    else GT
#endif

---------------------------------------------------------------------------------------
-- * Show1
---------------------------------------------------------------------------------------

liftShowsPrecDefault :: (GShow1 (Rep1 f), Generic1 f)
                     => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
liftShowsPrecDefault sp sl p = gliftShowsPrec sp sl p . from1

class GShow1 f where
  gliftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS

instance GShow1 f => GShow1 (D1 d f) where
  gliftShowsPrec sp sl p (M1 x) = gliftShowsPrec sp sl p x

instance (GShow1 f, GShow1 g) => GShow1 (f :+: g) where
  gliftShowsPrec sp sl p (L1 x) = gliftShowsPrec sp sl p x
  gliftShowsPrec sp sl p (R1 x) = gliftShowsPrec sp sl p x

instance (Constructor c, GShow1Con f, IsNullary f) => GShow1 (C1 c f) where
  gliftShowsPrec sp sl p c@(M1 x) = case fixity of
      Prefix -> showParen ( p > appPrec
                             && not ( isNullary x
                                      || conIsTuple c
#if __GLASGOW_HASKELL__ >= 711
                                      || conIsRecord c
#endif
                                    )
                           ) $
             (if conIsTuple c
                 then id
                 else let cn = conName c
                      in showParen (isInfixTypeCon cn) (showString cn))
           . (if isNullary x || conIsTuple c
                 then id
                 else showChar ' ')
           . showBraces t (gliftShowsPrecCon t sp sl appPrec1 x)
      Infix _ m -> showParen (p > m)
                    . showBraces t
                    $ gliftShowsPrecCon t sp sl (m+1) x
    where
      fixity :: Fixity
      fixity = conFixity c

      t :: ConType
      t = if conIsRecord c
          then Rec
          else case conIsTuple c of
              True  -> Tup
              False -> case fixity of
                  Prefix    -> Pref
                  Infix _ _ -> Inf $ conName c

showBraces :: ConType -> ShowS -> ShowS
showBraces Rec     b = showChar '{' . b . showChar '}'
showBraces Tup     b = showChar '(' . b . showChar ')'
showBraces Pref    b = b
showBraces (Inf _) b = b

conIsTuple :: Constructor c => C1 c f p -> Bool
conIsTuple = isTupleString . conName

isInfixTypeCon :: String -> Bool
isInfixTypeCon (':':_) = True
isInfixTypeCon _       = False

isTupleString :: String -> Bool
isTupleString ('(':',':_) = True
isTupleString _           = False

class GShow1Con f where
  gliftShowsPrecCon :: ConType -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                    -> Int -> f a   -> ShowS

data ConType = Rec | Tup | Pref | Inf String

instance GShow1Con V1 where
  gliftShowsPrecCon _ _ _ _ !_ = undefined

instance GShow1Con U1 where
  gliftShowsPrecCon _ _ _ _ U1 = id

instance GShow1Con Par1 where
  gliftShowsPrecCon _ sp _  p (Par1 x) = sp p x

instance Show c => GShow1Con (K1 i c) where
  gliftShowsPrecCon _ _ _ p (K1 x) = showsPrec p x

instance Show1 f => GShow1Con (Rec1 f) where
  gliftShowsPrecCon _ sp sl p (Rec1 x) = liftShowsPrec sp sl p x

instance (Selector s, GShow1Con f) => GShow1Con (S1 s f) where
  gliftShowsPrecCon t sp sl p sel@(M1 x)
    | selName sel == "" =   gliftShowsPrecCon t sp sl p x
    | otherwise         =   showString (selName sel)
                          . showString " = "
                          . gliftShowsPrecCon t sp sl 0 x

instance (GShow1Con f, GShow1Con g) => GShow1Con (f :*: g) where
  gliftShowsPrecCon t sp sl p (a :*: b) =
    case t of
         Rec ->     gliftShowsPrecCon t sp sl 0 a
                  . showString ", "
                  . gliftShowsPrecCon t sp sl 0 b

         Inf o ->   gliftShowsPrecCon t sp sl p a
                  . showSpace
                  . infixOp o
                  . showSpace
                  . gliftShowsPrecCon t sp sl p b

         Tup ->     gliftShowsPrecCon t sp sl 0 a
                  . showChar ','
                  . gliftShowsPrecCon t sp sl 0 b

         Pref ->    gliftShowsPrecCon t sp sl p a
                  . showSpace
                  . gliftShowsPrecCon t sp sl p b
    where
      infixOp :: String -> ShowS
      infixOp o = if isInfixTypeCon o
                     then showString o
                     else showChar '`' . showString o . showChar '`'

instance (Show1 f, GShow1Con g) => GShow1Con (f :.: g) where
  gliftShowsPrecCon t sp sl p (Comp1 x) =
    liftShowsPrec (gliftShowsPrecCon t sp sl)
                  (showListWith (gliftShowsPrecCon t sp sl 0))
                  p x

#if MIN_VERSION_base(4,9,0)
instance GShow1Con UChar where
  gliftShowsPrecCon _ _ _ p (UChar c)   = showsPrec (hashPrec p) (C# c) . oneHash

instance GShow1Con UDouble where
  gliftShowsPrecCon _ _ _ p (UDouble d) = showsPrec (hashPrec p) (D# d) . twoHash

instance GShow1Con UFloat where
  gliftShowsPrecCon _ _ _ p (UFloat f)  = showsPrec (hashPrec p) (F# f) . oneHash

instance GShow1Con UInt where
  gliftShowsPrecCon _ _ _ p (UInt i)    = showsPrec (hashPrec p) (I# i) . oneHash

instance GShow1Con UWord where
  gliftShowsPrecCon _ _ _ p (UWord w)   = showsPrec (hashPrec p) (W# w) . twoHash

oneHash, twoHash :: ShowS
hashPrec :: Int -> Int
# if __GLASGOW_HASKELL__ >= 711
oneHash  = showChar '#'
twoHash  = showString "##"
hashPrec = const 0
# else
oneHash  = id
twoHash  = id
hashPrec = id
# endif
#endif

class IsNullary f where
    -- Returns 'True' if the constructor has no fields.
    isNullary :: f a -> Bool

instance IsNullary U1 where
    isNullary _ = True

instance IsNullary Par1 where
    isNullary _ = False

instance IsNullary (K1 i c) where
    isNullary _ = False

instance IsNullary f => IsNullary (S1 s f) where
    isNullary (M1 x) = isNullary x

instance IsNullary (Rec1 f) where
    isNullary _ = False

instance IsNullary (f :*: g) where
    isNullary _ = False

instance IsNullary (f :.: g) where
    isNullary _ = False

#if MIN_VERSION_base(4,9,0)
instance IsNullary UChar where
    isNullary _ = False

instance IsNullary UDouble where
    isNullary _ = False

instance IsNullary UFloat where
    isNullary _ = False

instance IsNullary UInt where
    isNullary _ = False

instance IsNullary UWord where
    isNullary _ = False
#endif
