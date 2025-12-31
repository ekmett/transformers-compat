{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-|
Module:      Data.Functor.Classes.Generic
Copyright:   (C) 2015-2016 Edward Kmett, Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Internal functionality for "Data.Functor.Classes.Generic".

This is an internal module and, as such, the API is not guaranteed to remain the
same between any given release.
-}
module Data.Functor.Classes.Generic.Internal
  ( -- * Options
    Options(..)
  , defaultOptions
  , latestGHCOptions
    -- * 'Eq1'
  , liftEqDefault
  , liftEqOptions
  , GEq1(..)
    -- * 'Ord1'
  , liftCompareDefault
  , liftCompareOptions
  , GOrd1(..)
    -- * 'Read1'
  , liftReadsPrecDefault
  , liftReadsPrecOptions
  , GRead1(..)
  , GRead1Con(..)
    -- * 'Show1'
  , liftShowsPrecDefault
  , liftShowsPrecOptions
  , GShow1(..)
  , GShow1Con(..)
    -- * 'Eq'
  , eqDefault
  , GEq(..)
    -- * 'Ord'
  , compareDefault
  , GOrd(..)
    -- * 'Read'
  , readsPrecDefault
  , GRead(..)
    -- * 'Show'
  , showsPrecDefault
  , showsPrecOptions
  , GShow(..)
    -- * 'FunctorClassesDefault'
  , FunctorClassesDefault(..)
  -- * Miscellaneous types
  , ConType(..)
  , IsNullaryDataType(..)
  , IsNullaryCon(..)
  ) where

import Data.Char (isSymbol, ord)
import Data.Functor.Classes
import GHC.Exts
import GHC.Generics hiding (prec)
import GHC.Read (expectP, list, paren, parens)
import GHC.Show (appPrec, appPrec1, showSpace)
import Text.ParserCombinators.ReadPrec
import Text.Read (Read(..))
import Text.Read.Lex (Lexeme(..))
import Text.Show (showListWith)


-------------------------------------------------------------------------------
-- * Options
-------------------------------------------------------------------------------

-- | Options that further configure how the functions in
-- "Data.Functor.Classes.Generic" should behave. Currently, the 'Options' have
-- no effect (but this may change in the future).
data Options = Options

-- | Options that match the behavior of the installed version of GHC.
defaultOptions :: Options
defaultOptions = Options

-- | Options that match the behavior of the most recent GHC release.
latestGHCOptions :: Options
latestGHCOptions = Options

-------------------------------------------------------------------------------
-- * Eq
-------------------------------------------------------------------------------

-- | A default @('==')@ implementation for 'Generic1' instances that leverages
-- 'Eq1'.
eqDefault :: (GEq (Rep1 f a), Generic1 f) => f a -> f a -> Bool
eqDefault m n = geq (from1 m) (from1 n)

-- | Class of generic representation types that can be checked for equality.
class GEq a where
  geq :: a -> a -> Bool

instance Eq c => GEq (K1 i c p) where
  geq (K1 c) (K1 d) = c == d

instance (GEq (f p), GEq (g p)) => GEq ((f :*: g) p) where
  geq (a :*: b) (c :*: d) = geq a c && geq b d

instance (GEq (f p), GEq (g p)) => GEq ((f :+: g) p) where
  geq (L1 a) (L1 c) = geq a c
  geq (R1 b) (R1 d) = geq b d
  geq _      _      = False

instance GEq (f p) => GEq (M1 i c f p) where
  geq (M1 a) (M1 b) = geq a b

instance GEq (U1 p) where
  geq U1 U1 = True

instance GEq (V1 p) where
  geq _ _ = True

instance Eq p => GEq (Par1 p) where
  geq (Par1 a) (Par1 b) = a == b

instance (Eq1 f, Eq p) => GEq (Rec1 f p) where
  geq (Rec1 a) (Rec1 b) = eq1 a b

instance (Eq1 f, GEq (g p)) => GEq ((f :.: g) p) where
  geq (Comp1 m) (Comp1 n) = liftEq geq m n

-- Unboxed types
instance GEq (UAddr p) where
  geq = eqUAddr

instance GEq (UChar p) where
  geq = eqUChar

instance GEq (UDouble p) where
  geq = eqUDouble

instance GEq (UFloat p) where
  geq = eqUFloat

instance GEq (UInt p) where
  geq = eqUInt

instance GEq (UWord p) where
  geq = eqUWord

-------------------------------------------------------------------------------
-- * Eq1
-------------------------------------------------------------------------------

-- | A sensible default 'liftEq' implementation for 'Generic1' instances.
liftEqDefault :: (GEq1 (Rep1 f), Generic1 f)
              => (a -> b -> Bool) -> f a -> f b -> Bool
liftEqDefault = liftEqOptions defaultOptions

-- | Like 'liftEqDefault', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
liftEqOptions :: (GEq1 (Rep1 f), Generic1 f)
              => Options -> (a -> b -> Bool) -> f a -> f b -> Bool
liftEqOptions _ f m n = gliftEq f (from1 m) (from1 n)

-- | Class of generic representation types that can lift equality through unary
-- type constructors.
class
#if __GLASGOW_HASKELL__ >= 806
    (forall a. Eq a => GEq (t a)) =>
#endif
    GEq1 t where
  gliftEq :: (a -> b -> Bool) -> t a -> t b -> Bool

instance Eq c => GEq1 (K1 i c) where
  gliftEq _ (K1 c) (K1 d) = c == d

instance (GEq1 f, GEq1 g) => GEq1 (f :*: g) where
  gliftEq f (a :*: b) (c :*: d) = gliftEq f a c && gliftEq f b d

instance (GEq1 f, GEq1 g) => GEq1 (f :+: g) where
  gliftEq f (L1 a) (L1 c) = gliftEq f a c
  gliftEq f (R1 b) (R1 d) = gliftEq f b d
  gliftEq _ _      _      = False

instance GEq1 f => GEq1 (M1 i c f) where
  gliftEq f (M1 a) (M1 b) = gliftEq f a b

instance GEq1 U1 where
  gliftEq _ U1 U1 = True

instance GEq1 V1 where
  gliftEq _ _ _ = True

instance GEq1 Par1 where
  gliftEq f (Par1 a) (Par1 b) = f a b

instance Eq1 f => GEq1 (Rec1 f) where
  gliftEq f (Rec1 a) (Rec1 b) = liftEq f a b

instance (Eq1 f, GEq1 g) => GEq1 (f :.: g) where
  gliftEq f (Comp1 m) (Comp1 n) = liftEq (gliftEq f) m n

-- Unboxed types
instance GEq1 UAddr where
  gliftEq _ = eqUAddr

instance GEq1 UChar where
  gliftEq _ = eqUChar

instance GEq1 UDouble where
  gliftEq _ = eqUDouble

instance GEq1 UFloat where
  gliftEq _ = eqUFloat

instance GEq1 UInt where
  gliftEq _ = eqUInt

instance GEq1 UWord where
  gliftEq _ = eqUWord

eqUAddr :: UAddr p -> UAddr q -> Bool
eqUAddr (UAddr a1) (UAddr a2) = isTrue# (eqAddr# a1 a2)

eqUChar :: UChar p -> UChar q -> Bool
eqUChar (UChar c1) (UChar c2) = isTrue# (eqChar# c1 c2)

eqUDouble :: UDouble p -> UDouble q -> Bool
eqUDouble (UDouble d1) (UDouble d2) = isTrue# (d1 ==## d2)

eqUFloat :: UFloat p -> UFloat q -> Bool
eqUFloat (UFloat f1) (UFloat f2) = isTrue# (eqFloat# f1 f2)

eqUInt :: UInt p -> UInt q -> Bool
eqUInt (UInt i1) (UInt i2) = isTrue# (i1 ==# i2)

eqUWord :: UWord p -> UWord q -> Bool
eqUWord (UWord w1) (UWord w2) = isTrue# (eqWord# w1 w2)

-------------------------------------------------------------------------------
-- * Ord
-------------------------------------------------------------------------------

-- | A default 'compare' implementation for 'Generic1' instances that leverages
-- 'Ord1'.
compareDefault :: (GOrd (Rep1 f a), Generic1 f) => f a -> f a -> Ordering
compareDefault m n = gcompare (from1 m) (from1 n)

-- | Class of generic representation types that can be totally ordered.
class GEq a => GOrd a where
  gcompare :: a -> a -> Ordering

instance Ord c => GOrd (K1 i c p) where
  gcompare (K1 c) (K1 d) = compare c d

instance (GOrd (f p), GOrd (g p)) => GOrd ((f :*: g) p) where
  gcompare (a :*: b) (c :*: d) = gcompare a c `mappend` gcompare b d

instance (GOrd (f p), GOrd (g p)) => GOrd ((f :+: g) p) where
  gcompare (L1 a) (L1 c) = gcompare a c
  gcompare L1{}   R1{}   = LT
  gcompare R1{}   L1{}   = GT
  gcompare (R1 b) (R1 d) = gcompare b d

instance GOrd (f p) => GOrd (M1 i c f p) where
  gcompare (M1 a) (M1 b) = gcompare a b

instance GOrd (U1 p) where
  gcompare U1 U1 = EQ

instance GOrd (V1 p) where
  gcompare _ _ = EQ

instance Ord p => GOrd (Par1 p) where
  gcompare (Par1 a) (Par1 b) = compare a b

instance (Ord1 f, Ord p) => GOrd (Rec1 f p) where
  gcompare (Rec1 a) (Rec1 b) = compare1 a b

instance (Ord1 f, GOrd (g p)) => GOrd ((f :.: g) p) where
  gcompare (Comp1 m) (Comp1 n) = liftCompare gcompare m n

-- Unboxed types
instance GOrd (UAddr p) where
  gcompare = compareUAddr

instance GOrd (UChar p) where
  gcompare = compareUChar

instance GOrd (UDouble p) where
  gcompare = compareUDouble

instance GOrd (UFloat p) where
  gcompare = compareUFloat

instance GOrd (UInt p) where
  gcompare = compareUInt

instance GOrd (UWord p) where
  gcompare = compareUWord

-------------------------------------------------------------------------------
-- * Ord1
-------------------------------------------------------------------------------

-- | A sensible default 'liftCompare' implementation for 'Generic1' instances.
liftCompareDefault :: (GOrd1 (Rep1 f), Generic1 f)
                   => (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareDefault = liftCompareOptions defaultOptions

-- | Like 'liftCompareDefault', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
liftCompareOptions :: (GOrd1 (Rep1 f), Generic1 f)
                   => Options -> (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareOptions _ f m n = gliftCompare f (from1 m) (from1 n)

-- | Class of generic representation types that can lift a total order through
-- unary type constructors.
class ( GEq1 t
#if __GLASGOW_HASKELL__ >= 806
      , forall a. Ord a => GOrd (t a)
#endif
      ) => GOrd1 t where
  gliftCompare :: (a -> b -> Ordering) -> t a -> t b -> Ordering

instance Ord c => GOrd1 (K1 i c) where
  gliftCompare _ (K1 c) (K1 d) = compare c d

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :*: g) where
  gliftCompare f (a :*: b) (c :*: d) =
    gliftCompare f a c `mappend` gliftCompare f b d

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :+: g) where
  gliftCompare f (L1 a) (L1 c) = gliftCompare f a c
  gliftCompare _ L1{}   R1{}   = LT
  gliftCompare _ R1{}   L1{}   = GT
  gliftCompare f (R1 b) (R1 d) = gliftCompare f b d

instance GOrd1 f => GOrd1 (M1 i c f) where
  gliftCompare f (M1 a) (M1 b) = gliftCompare f a b

instance GOrd1 U1 where
  gliftCompare _ U1 U1 = EQ

instance GOrd1 V1 where
  gliftCompare _ _ _ = EQ

instance GOrd1 Par1 where
  gliftCompare f (Par1 a) (Par1 b) = f a b

instance Ord1 f => GOrd1 (Rec1 f) where
  gliftCompare f (Rec1 a) (Rec1 b) = liftCompare f a b

instance (Ord1 f, GOrd1 g) => GOrd1 (f :.: g) where
  gliftCompare f (Comp1 m) (Comp1 n) = liftCompare (gliftCompare f) m n

-- Unboxed types
instance GOrd1 UAddr where
  gliftCompare _ = compareUAddr

instance GOrd1 UChar where
  gliftCompare _ = compareUChar

instance GOrd1 UDouble where
  gliftCompare _ = compareUDouble

instance GOrd1 UFloat where
  gliftCompare _ = compareUFloat

instance GOrd1 UInt where
  gliftCompare _ = compareUInt

instance GOrd1 UWord where
  gliftCompare _ = compareUWord

compareUAddr :: UAddr p -> UAddr q -> Ordering
compareUAddr (UAddr a1) (UAddr a2) = primCompare (eqAddr# a1 a2) (leAddr# a1 a2)

compareUChar :: UChar p -> UChar q -> Ordering
compareUChar (UChar c1) (UChar c2) = primCompare (eqChar# c1 c2) (leChar# c1 c2)

compareUDouble :: UDouble p -> UDouble q -> Ordering
compareUDouble (UDouble d1) (UDouble d2) = primCompare (d1 ==## d2) (d1 <=## d2)

compareUFloat :: UFloat p -> UFloat q -> Ordering
compareUFloat (UFloat f1) (UFloat f2) = primCompare (eqFloat# f1 f2) (leFloat# f1 f2)

compareUInt :: UInt p -> UInt q -> Ordering
compareUInt (UInt i1) (UInt i2) = primCompare (i1 ==# i2) (i1 <=# i2)

compareUWord :: UWord p -> UWord q -> Ordering
compareUWord (UWord w1) (UWord w2) = primCompare (eqWord# w1 w2) (leWord# w1 w2)

primCompare :: Int# -> Int# -> Ordering
primCompare eq le = if isTrue# eq then EQ
                    else if isTrue# le then LT
                    else GT

-------------------------------------------------------------------------------
-- * Read
-------------------------------------------------------------------------------

-- | A default 'readsPrec' implementation for 'Generic1' instances that leverages
-- 'Read1'.
readsPrecDefault :: (GRead (Rep1 f a), Generic1 f) => Int -> ReadS (f a)
readsPrecDefault p = readPrec_to_S (fmap to1 greadPrec) p

-- | Class of generic representation types that can be parsed from a 'String'.
class GRead a where
  greadPrec :: ReadPrec a

instance (GRead (f p), IsNullaryDataType f) => GRead (D1 d f p) where
  greadPrec = d1ReadPrec greadPrec

instance GRead (V1 p) where
  greadPrec = pfail

instance (GRead (f p), GRead (g p)) => GRead ((f :+: g) p) where
  greadPrec = fmap L1 greadPrec +++ fmap R1 greadPrec

instance (Constructor c, GReadCon (f p), IsNullaryCon f) => GRead (C1 c f p) where
  greadPrec = c1ReadPrec greadPrecCon

-- | Class of generic representation types that can be parsed from a 'String',
-- and for which the 'ConType' has been determined.
class GReadCon a where
  greadPrecCon :: ConType -> ReadPrec a

instance GReadCon (U1 p) where
  greadPrecCon _ = return U1

instance Read c => GReadCon (K1 i c p) where
  greadPrecCon _ = coerceK1 readPrec

instance (Selector s, GReadCon (f p)) => GReadCon (S1 s f p) where
  greadPrecCon = s1ReadPrec . greadPrecCon

instance (GReadCon (f p), GReadCon (g p)) => GReadCon ((f :*: g) p) where
  greadPrecCon t = productReadPrec t (greadPrecCon t) (greadPrecCon t)

instance Read p => GReadCon (Par1 p) where
  greadPrecCon _ = coercePar1 readPrec

instance (Read1 f, Read p) => GReadCon (Rec1 f p) where
  greadPrecCon _ = coerceRec1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S readPrec) (readPrec_to_S readListPrec 0)

instance (Read1 f, GReadCon (g p)) => GReadCon ((f :.: g) p) where
  greadPrecCon t = coerceComp1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S       grpc)
                    (readPrec_to_S (list grpc) 0)
    where
      grpc = greadPrecCon t

-------------------------------------------------------------------------------
-- * Read1
-------------------------------------------------------------------------------

-- | A sensible default 'liftReadsPrec' implementation for 'Generic1' instances.
liftReadsPrecDefault :: (GRead1 (Rep1 f), Generic1 f)
                     => (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
liftReadsPrecDefault = liftReadsPrecOptions defaultOptions

-- | Like 'liftReadsPrecDefault', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
liftReadsPrecOptions :: (GRead1 (Rep1 f), Generic1 f)
                     => Options -> (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
liftReadsPrecOptions _ rp rl p =
  readPrec_to_S (fmap to1 $ gliftReadPrec
                                      (readS_to_Prec rp)
                                      (readS_to_Prec (const rl))) p

coerceM1 :: ReadPrec (f p) -> ReadPrec (M1 i c f p)
coerceM1 = coerce

coercePar1 :: ReadPrec p -> ReadPrec (Par1 p)
coercePar1 = coerce

coerceRec1 :: ReadPrec (f a) -> ReadPrec (Rec1 f a)
coerceRec1 = coerce

coerceComp1 :: ReadPrec (f (g a)) -> ReadPrec ((f :.: g) a)
coerceComp1 = coerce

coerceK1 :: ReadPrec c -> ReadPrec (K1 i c p)
coerceK1 = coerce

isSymVar :: String -> Bool
isSymVar ""    = False
isSymVar (c:_) = startsVarSym c

startsVarSym :: Char -> Bool
startsVarSym c = startsVarSymASCII c || (ord c > 0x7f && isSymbol c) -- Infix Ids

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"

snocView :: [a] -> Maybe ([a],a)
        -- Split off the last element
snocView [] = Nothing
snocView xs = go [] xs
  where
      -- Invariant: second arg is non-empty
    go acc [a]    = Just (reverse acc, a)
    go acc (a:as) = go (a:acc) as
    go _ [] = error "Util: snocView"

identHLexemes :: String -> [Lexeme]
identHLexemes s | Just (ss, '#') <- snocView s = [Ident ss, Symbol "#"]
                | otherwise                    = [Ident s]

-- | Class of generic representation types for unary type constructors that can
-- be parsed from a 'String'.
class
#if __GLASGOW_HASKELL__ >= 806
    (forall a. Read a => GRead (f a)) =>
#endif
    GRead1 f where
  gliftReadPrec :: ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)

instance (GRead1 f, IsNullaryDataType f) => GRead1 (D1 d f) where
  gliftReadPrec rp rl = d1ReadPrec $ gliftReadPrec rp rl

d1ReadPrec :: forall d f p. IsNullaryDataType f
           => ReadPrec (f p) -> ReadPrec (D1 d f p)
d1ReadPrec rp = coerceM1 $ parensIfNonNullary rp
  where
    x :: f p
    x = undefined

    parensIfNonNullary :: ReadPrec a -> ReadPrec a
    parensIfNonNullary = if isNullaryDataType x
                            then id
                            else parens

instance GRead1 V1 where
  gliftReadPrec _ _ = pfail

instance (GRead1 f, GRead1 g) => GRead1 (f :+: g) where
  gliftReadPrec rp rl =
    fmap L1 (gliftReadPrec rp rl) +++ fmap R1 (gliftReadPrec rp rl)

instance (Constructor c, GRead1Con f, IsNullaryCon f) => GRead1 (C1 c f) where
  gliftReadPrec rp rl = c1ReadPrec $ \t -> gliftReadPrecCon t rp rl

c1ReadPrec :: forall c f p. (Constructor c, IsNullaryCon f)
           => (ConType -> ReadPrec (f p)) -> ReadPrec (C1 c f p)
c1ReadPrec rpc =
  coerceM1 $ case fixity of
    Prefix -> precIfNonNullary $ do
                if conIsTuple c
                   then return ()
                   else let cn = conName c
                        in if isInfixDataCon cn
                              then readSurround '(' (expectP (Symbol cn)) ')'
                              else mapM_ expectP $ identHLexemes cn
                readBraces t (rpc t)
    Infix _ m -> prec m $ rpc t
  where
    c :: C1 c f p
    c = undefined

    x :: f p
    x = undefined

    fixity :: Fixity
    fixity = conFixity c

    precIfNonNullary :: ReadPrec a -> ReadPrec a
    precIfNonNullary = if isNullaryCon x
                          then id
                          else prec (if conIsRecord c
                                        then appPrec1
                                        else appPrec)

    t :: ConType
    t = if conIsRecord c
        then Rec
        else case conIsTuple c of
            True  -> Tup
            False -> case fixity of
                Prefix    -> Pref
                Infix _ _ -> Inf $ conName c

readBraces :: ConType -> ReadPrec a -> ReadPrec a
readBraces Rec     r = readSurround '{' r '}'
readBraces Tup     r = paren r
readBraces Pref    r = r
readBraces (Inf _) r = r

readSurround :: Char -> ReadPrec a -> Char -> ReadPrec a
readSurround c1 r c2 = do
  expectP (Punc [c1])
  r' <- r
  expectP (Punc [c2])
  return r'

-- | Class of generic representation types for unary type constructors that
-- can be parsed from a 'String', and for which the 'ConType' has been
-- determined.
class
#if __GLASGOW_HASKELL__ >= 806
    (forall a. Read a => GReadCon (f a)) =>
#endif
    GRead1Con f where
  gliftReadPrecCon :: ConType -> ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)

instance GRead1Con U1 where
  gliftReadPrecCon _ _ _ = return U1

instance Read c => GRead1Con (K1 i c) where
  gliftReadPrecCon _ _ _ = coerceK1 readPrec

instance (Selector s, GRead1Con f) => GRead1Con (S1 s f) where
  gliftReadPrecCon t rp rl = s1ReadPrec $ gliftReadPrecCon t rp rl

s1ReadPrec :: forall s f p. Selector s
           => ReadPrec (f p) -> ReadPrec (S1 s f p)
s1ReadPrec rp
  | selectorName == "" = coerceM1 $ step rp
  | otherwise          = coerceM1 $ do
                            mapM_ expectP $ readLblLexemes selectorName
                            expectP (Punc "=")
                            reset rp
  where
    selectorName :: String
    selectorName = selName (undefined :: S1 s f p)

    readLblLexemes :: String -> [Lexeme]
    readLblLexemes lbl | isSymVar lbl
                       = [Punc "(", Symbol lbl, Punc ")"]
                       | otherwise
                       = identHLexemes lbl

instance (GRead1Con f, GRead1Con g) => GRead1Con (f :*: g) where
  gliftReadPrecCon t rp rl =
    productReadPrec t (gliftReadPrecCon t rp rl) (gliftReadPrecCon t rp rl)

productReadPrec :: ConType -> ReadPrec (f p) -> ReadPrec (g p) -> ReadPrec ((f :*: g) p)
productReadPrec t rpf rpg = do
    l <- rpf
    case t of
         Rec   -> expectP (Punc ",")
         Inf o -> infixPrec o
         Tup   -> expectP (Punc ",")
         Pref  -> return ()
    r <- rpg
    return (l :*: r)
  where
    infixPrec :: String -> ReadPrec ()
    infixPrec o = if isInfixDataCon o
                     then expectP (Symbol o)
                     else mapM_ expectP $
                              [Punc "`"] ++ identHLexemes o ++ [Punc "`"]

instance GRead1Con Par1 where
  gliftReadPrecCon _ rp _ = coercePar1 rp

instance Read1 f => GRead1Con (Rec1 f) where
  gliftReadPrecCon _ rp rl = coerceRec1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S rp) (readPrec_to_S rl 0)

instance (Read1 f, GRead1Con g) => GRead1Con (f :.: g) where
  gliftReadPrecCon t rp rl = coerceComp1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S       grpc)
                    (readPrec_to_S (list grpc) 0)
    where
      grpc = gliftReadPrecCon t rp rl

-------------------------------------------------------------------------------
-- * Show
-------------------------------------------------------------------------------

-- | A default 'showsPrec' implementation for 'Generic1' instances that leverages
-- 'Show1'.
showsPrecDefault :: (GShow (Rep1 f a), Generic1 f)
                 => Int -> f a -> ShowS
showsPrecDefault = showsPrecOptions defaultOptions

-- | Like 'showsPrecDefault', but with configurable 'Options'. Currently, the
-- 'Options' have no effect (but this may change in the future).
showsPrecOptions :: (GShow (Rep1 f a), Generic1 f)
                 => Options -> Int -> f a -> ShowS
showsPrecOptions _ p = gshowsPrec p . from1

-- | Class of generic representation types that can be converted to a 'String'.
class GShow a where
  gshowsPrec :: Int -> a -> ShowS

instance GShow (f p) => GShow (D1 d f p) where
  gshowsPrec p (M1 x) = gshowsPrec p x

instance GShow (V1 p) where
  gshowsPrec = v1ShowsPrec

instance (GShow (f p), GShow (g p)) => GShow ((f :+: g) p) where
  gshowsPrec p (L1 x) = gshowsPrec p x
  gshowsPrec p (R1 x) = gshowsPrec p x

instance (Constructor c, GShowCon (f p), IsNullaryCon f) => GShow (C1 c f p) where
  gshowsPrec = c1ShowsPrec gshowsPrecCon

-- | Class of generic representation types that can be converted to a 'String', and
-- for which the 'ConType' has been determined.
class GShowCon a where
  gshowsPrecCon :: ConType -> Int -> a -> ShowS

instance GShowCon (U1 p) where
  gshowsPrecCon _ _ U1 = id

instance Show c => GShowCon (K1 i c p) where
  gshowsPrecCon _ p (K1 x) = showsPrec p x

instance (Selector s, GShowCon (f p)) => GShowCon (S1 s f p) where
  gshowsPrecCon = s1ShowsPrec . gshowsPrecCon

instance (GShowCon (f p), GShowCon (g p)) => GShowCon ((f :*: g) p) where
  gshowsPrecCon t = productShowsPrec (gshowsPrecCon t) (gshowsPrecCon t) t

instance Show p => GShowCon (Par1 p) where
  gshowsPrecCon _ p (Par1 x) = showsPrec p x

instance (Show1 f, Show p) => GShowCon (Rec1 f p) where
  gshowsPrecCon _ p (Rec1 x) = liftShowsPrec showsPrec showList p x

instance (Show1 f, GShowCon (g p)) => GShowCon ((f :.: g) p) where
  gshowsPrecCon t p (Comp1 x) =
    let glspc = gshowsPrecCon t
    in liftShowsPrec glspc (showListWith (glspc 0)) p x

instance GShowCon (UChar p) where
  gshowsPrecCon _ = uCharShowsPrec

instance GShowCon (UDouble p) where
  gshowsPrecCon _ = uDoubleShowsPrec

instance GShowCon (UFloat p) where
  gshowsPrecCon _ = uFloatShowsPrec

instance GShowCon (UInt p) where
  gshowsPrecCon _ = uIntShowsPrec

instance GShowCon (UWord p) where
  gshowsPrecCon _ = uWordShowsPrec

-------------------------------------------------------------------------------
-- * Show1
-------------------------------------------------------------------------------

-- | A sensible default 'liftShowsPrec' implementation for 'Generic1' instances.
liftShowsPrecDefault :: (GShow1 (Rep1 f), Generic1 f)
                     => (Int -> a -> ShowS) -> ([a] -> ShowS)
                     -> Int -> f a -> ShowS
liftShowsPrecDefault = liftShowsPrecOptions defaultOptions

-- | Like 'liftShowsPrecDefault', but with configurable 'Options'. Currently,
-- the 'Options' have no effect (but this may change in the future).
liftShowsPrecOptions :: (GShow1 (Rep1 f), Generic1 f)
                     => Options -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                     -> Int -> f a -> ShowS
liftShowsPrecOptions _ sp sl p = gliftShowsPrec sp sl p . from1

-- | Class of generic representation types for unary type constructors that can
-- be converted to a 'String'.
class
#if __GLASGOW_HASKELL__ >= 806
    (forall a. Show a => GShow (f a)) =>
#endif
    GShow1 f where
  gliftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS)
                 -> Int -> f a -> ShowS

instance GShow1 f => GShow1 (D1 d f) where
  gliftShowsPrec sp sl p (M1 x) = gliftShowsPrec sp sl p x

instance GShow1 V1 where
  gliftShowsPrec _ _ = v1ShowsPrec

v1ShowsPrec :: Int -> V1 p -> ShowS
v1ShowsPrec _ x = case x of {}

instance (GShow1 f, GShow1 g) => GShow1 (f :+: g) where
  gliftShowsPrec sp sl p (L1 x) = gliftShowsPrec sp sl p x
  gliftShowsPrec sp sl p (R1 x) = gliftShowsPrec sp sl p x

instance (Constructor c, GShow1Con f, IsNullaryCon f) => GShow1 (C1 c f) where
  gliftShowsPrec sp sl = c1ShowsPrec $ \t -> gliftShowsPrecCon t sp sl

c1ShowsPrec :: (Constructor c, IsNullaryCon f)
            => (ConType -> Int -> f p -> ShowS) -> Int -> C1 c f p -> ShowS
c1ShowsPrec sp p c@(M1 x) = case fixity of
    Prefix -> showParen ( p > appPrec
                           && not (isNullaryCon x || conIsTuple c)
                         ) $
           (if conIsTuple c
               then id
               else let cn = conName c
                    in showParen (isInfixDataCon cn) (showString cn))
         . (if isNullaryCon x || conIsTuple c
               then id
               else showChar ' ')
         . showBraces t (sp t appPrec1 x)
    Infix _ m -> showParen (p > m) $ sp t (m+1) x
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

-- | Class of generic representation types for unary type constructors that can
-- be converted to a 'String', and for which the 'ConType' has been determined.
class
#if __GLASGOW_HASKELL__ >= 806
    (forall a. Show a => GShowCon (f a)) =>
#endif
    GShow1Con f where
  gliftShowsPrecCon :: ConType -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                    -> Int -> f a -> ShowS

instance GShow1Con U1 where
  gliftShowsPrecCon _ _ _ _ U1 = id

instance Show c => GShow1Con (K1 i c) where
  gliftShowsPrecCon _ _ _ p (K1 x) = showsPrec p x

instance (Selector s, GShow1Con f) => GShow1Con (S1 s f) where
  gliftShowsPrecCon t sp sl = s1ShowsPrec $ gliftShowsPrecCon t sp sl

s1ShowsPrec :: Selector s => (Int -> f p -> ShowS) -> Int -> S1 s f p -> ShowS
s1ShowsPrec sp p sel@(M1 x)
  | selName sel == "" =   sp p x
  | otherwise         =   infixRec
                        . showString " = "
                        . sp 0 x
  where
    infixRec :: ShowS
    infixRec | isSymVar selectorName
             = showChar '(' . showString selectorName . showChar ')'
             | otherwise
             = showString selectorName

    selectorName :: String
    selectorName = selName sel

instance (GShow1Con f, GShow1Con g) => GShow1Con (f :*: g) where
  gliftShowsPrecCon t sp sl =
    productShowsPrec (gliftShowsPrecCon t sp sl)
                     (gliftShowsPrecCon t sp sl)
                     t

productShowsPrec :: (Int -> f p -> ShowS) -> (Int -> g p -> ShowS)
                 -> ConType -> Int -> (f :*: g) p -> ShowS
productShowsPrec spf spg t p (a :*: b) =
  case t of
       Rec ->     spf 0 a
                . showString ", "
                . spg 0 b

       Inf o ->   spf p a
                . showSpace
                . infixOp o
                . showSpace
                . spg p b

       Tup ->     spf 0 a
                . showChar ','
                . spg 0 b

       Pref ->    spf p a
                . showSpace
                . spg p b
  where
    infixOp :: String -> ShowS
    infixOp o = if isInfixDataCon o
                   then showString o
                   else showChar '`' . showString o . showChar '`'

instance GShow1Con Par1 where
  gliftShowsPrecCon _ sp _ p (Par1 x) = sp p x

instance Show1 f => GShow1Con (Rec1 f) where
  gliftShowsPrecCon _ sp sl p (Rec1 x) = liftShowsPrec sp sl p x

instance (Show1 f, GShow1Con g) => GShow1Con (f :.: g) where
  gliftShowsPrecCon t sp sl p (Comp1 x) =
    let glspc = gliftShowsPrecCon t sp sl
    in liftShowsPrec glspc (showListWith (glspc 0)) p x

instance GShow1Con UChar where
  gliftShowsPrecCon _ _ _ = uCharShowsPrec

instance GShow1Con UDouble where
  gliftShowsPrecCon _ _ _ = uDoubleShowsPrec

instance GShow1Con UFloat where
  gliftShowsPrecCon _ _ _ = uFloatShowsPrec

instance GShow1Con UInt where
  gliftShowsPrecCon _ _ _ = uIntShowsPrec

instance GShow1Con UWord where
  gliftShowsPrecCon _ _ _ = uWordShowsPrec

uCharShowsPrec :: Int -> UChar p -> ShowS
uCharShowsPrec p (UChar c) = shows (C# c) . oneHash

uDoubleShowsPrec :: Int -> UDouble p -> ShowS
uDoubleShowsPrec p (UDouble d) = shows (D# d) . twoHash

uFloatShowsPrec :: Int -> UFloat p -> ShowS
uFloatShowsPrec p (UFloat f) = shows (F# f) . oneHash

uIntShowsPrec :: Int -> UInt p -> ShowS
uIntShowsPrec p (UInt i) = shows (I# i) . oneHash

uWordShowsPrec :: Int -> UWord p -> ShowS
uWordShowsPrec p (UWord w) = shows (W# w) . twoHash

oneHash, twoHash :: ShowS
oneHash = showChar '#'
twoHash = showString "##"

-------------------------------------------------------------------------------
-- * GenericFunctorClasses
-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@. Its 'Eq1', 'Ord1',
-- 'Read1', and 'Show1' instances leverage 'Generic1'-based defaults.
newtype FunctorClassesDefault f a =
  FunctorClassesDefault { getFunctorClassesDefault :: f a }

instance (GEq1 (Rep1 f), Generic1 f) => Eq1 (FunctorClassesDefault f) where
   liftEq f (FunctorClassesDefault x) (FunctorClassesDefault y) = liftEqDefault f x y
instance (GOrd1 (Rep1 f), Generic1 f) => Ord1 (FunctorClassesDefault f) where
   liftCompare f (FunctorClassesDefault x) (FunctorClassesDefault y) = liftCompareDefault f x y
instance (GRead1 (Rep1 f), Generic1 f) => Read1 (FunctorClassesDefault f) where
   liftReadsPrec rp rl p = coerceFCD (liftReadsPrecDefault rp rl p)
instance (GShow1 (Rep1 f), Generic1 f) => Show1 (FunctorClassesDefault f) where
   liftShowsPrec sp sl p (FunctorClassesDefault x) = liftShowsPrecDefault sp sl p x

instance (GEq (Rep1 f a), Generic1 f) => Eq (FunctorClassesDefault f a) where
  FunctorClassesDefault x == FunctorClassesDefault y = eqDefault x y
instance (GOrd (Rep1 f a), Generic1 f) => Ord (FunctorClassesDefault f a) where
  compare (FunctorClassesDefault x) (FunctorClassesDefault y) = compareDefault x y
instance (GRead (Rep1 f a), Generic1 f) => Read (FunctorClassesDefault f a) where
  readsPrec p = coerceFCD (readsPrecDefault p)
instance (GShow (Rep1 f a), Generic1 f) => Show (FunctorClassesDefault f a) where
  showsPrec p (FunctorClassesDefault x) = showsPrecDefault p x

coerceFCD :: ReadS (f a) -> ReadS (FunctorClassesDefault f a)
coerceFCD = coerce

-------------------------------------------------------------------------------
-- * Shared code
-------------------------------------------------------------------------------

-- | Whether a constructor is a record ('Rec'), a tuple ('Tup'), is prefix ('Pref'),
-- or infix ('Inf').
data ConType = Rec | Tup | Pref | Inf String

conIsTuple :: Constructor c => C1 c f p -> Bool
conIsTuple = isTupleString . conName

isTupleString :: String -> Bool
isTupleString ('(':',':_) = True
isTupleString _           = False

isInfixDataCon :: String -> Bool
isInfixDataCon (':':_) = True
isInfixDataCon _       = False

-- | Class of generic representation types that represent a data type with
-- zero or more constructors.
class IsNullaryDataType f where
    -- | Returns 'True' if the data type has no constructors.
    isNullaryDataType :: f a -> Bool

instance IsNullaryDataType (f :+: g) where
    isNullaryDataType _ = False

instance IsNullaryDataType (C1 c f) where
    isNullaryDataType _ = False

-- | Class of generic representation types that represent a constructor with
-- zero or more fields.
class IsNullaryCon f where
    -- | Returns 'True' if the constructor has no fields.
    isNullaryCon :: f a -> Bool

instance IsNullaryDataType V1 where
    isNullaryDataType _ = True

instance IsNullaryCon U1 where
    isNullaryCon _ = True

instance IsNullaryCon Par1 where
    isNullaryCon _ = False

instance IsNullaryCon (K1 i c) where
    isNullaryCon _ = False

instance IsNullaryCon f => IsNullaryCon (S1 s f) where
    isNullaryCon (M1 x) = isNullaryCon x

instance IsNullaryCon (Rec1 f) where
    isNullaryCon _ = False

instance IsNullaryCon (f :*: g) where
    isNullaryCon _ = False

instance IsNullaryCon (f :.: g) where
    isNullaryCon _ = False

instance IsNullaryCon UChar where
    isNullaryCon _ = False

instance IsNullaryCon UDouble where
    isNullaryCon _ = False

instance IsNullaryCon UFloat where
    isNullaryCon _ = False

instance IsNullaryCon UInt where
    isNullaryCon _ = False

instance IsNullaryCon UWord where
    isNullaryCon _ = False
