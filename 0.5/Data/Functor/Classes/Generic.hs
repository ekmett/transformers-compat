{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  ) where

import Data.Functor.Classes
#ifdef GENERIC_DERIVING
import Generics.Deriving.Base hiding (prec)
#else
import GHC.Generics hiding (prec)
#endif
import GHC.Read (list, paren, parens)
import GHC.Show (appPrec, appPrec1, showSpace)
import Text.ParserCombinators.ReadPrec
import Text.Read (Read(..))
import Text.Read.Lex (Lexeme(..))
import Text.Show (showListWith)

#if MIN_VERSION_base(4,7,0)
import Data.Coerce (coerce)
import GHC.Read (expectP)
#else
import GHC.Read (lexP)
import Unsafe.Coerce (unsafeCoerce)
#endif

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
import GHC.Exts
#endif

---------------------------------------------------------------------------------------
-- * Options
---------------------------------------------------------------------------------------

-- | Options that further configure how the functions in
-- "Data.Functor.Classes.Generic" should behave.
newtype Options = Options
  { ghc8ShowBehavior :: Bool
    -- ^ If 'True', a default 'Show1' implementation will show hash signs
    -- (@#@) when showing unlifted types.
  }

-- | Options that match the behavior of the installed version of GHC.
defaultOptions :: Options
defaultOptions = Options
  {
#if __GLASGOW_HASKELL__ >= 800
  ghc8ShowBehavior = True
#else
  ghc8ShowBehavior = False
#endif
  }

-- | Options that match the behavior of the most recent GHC release.
latestGHCOptions :: Options
latestGHCOptions = Options { ghc8ShowBehavior = True }

---------------------------------------------------------------------------------------
-- * Eq1
---------------------------------------------------------------------------------------

-- | A sensible default 'liftEq' implementation for 'Generic1' instances.
liftEqDefault :: (GEq1 (Rep1 f), Generic1 f)
              => (a -> b -> Bool) -> f a -> f b -> Bool
liftEqDefault = liftEqOptions defaultOptions

-- | Like 'liftEqDefault', but with configurable 'Options'.
liftEqOptions :: (GEq1 (Rep1 f), Generic1 f)
              => Options -> (a -> b -> Bool) -> f a -> f b -> Bool
liftEqOptions opts f m n = gliftEq opts f (from1 m) (from1 n)

class GEq1 t where
  gliftEq :: Options -> (a -> b -> Bool) -> t a -> t b -> Bool

instance Eq c => GEq1 (K1 i c) where
  gliftEq _ _ (K1 c) (K1 d) = c == d

instance (GEq1 f, GEq1 g) => GEq1 (f :*: g) where
  gliftEq opts f (a :*: b) (c :*: d) = gliftEq opts f a c && gliftEq opts f b d

instance (Eq1 f, GEq1 g) => GEq1 (f :.: g) where
  gliftEq opts f (Comp1 m) (Comp1 n) = liftEq (gliftEq opts f) m n

instance (GEq1 f, GEq1 g) => GEq1 (f :+: g) where
  gliftEq opts f (L1 a) (L1 c) = gliftEq opts f a c
  gliftEq opts f (R1 b) (R1 d) = gliftEq opts f b d
  gliftEq _    _ _      _      = False

instance GEq1 f => GEq1 (M1 i c f) where
  gliftEq opts f (M1 a) (M1 b) = gliftEq opts f a b

instance GEq1 U1 where
  gliftEq _ _ U1 U1 = True

instance GEq1 V1 where
  gliftEq _ _ !_ = undefined

instance GEq1 Par1 where
  gliftEq _ f (Par1 a) (Par1 b) = f a b

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
-- Unboxed types
instance GEq1 UAddr where
  gliftEq _ _ (UAddr a1) (UAddr a2) = isTrue# (eqAddr# a1 a2)

instance GEq1 UChar where
  gliftEq _ _ (UChar c1) (UChar c2) = isTrue# (eqChar# c1 c2)

instance GEq1 UDouble where
  gliftEq _ _ (UDouble d1) (UDouble d2) = isTrue# (d1 ==## d2)

instance GEq1 UFloat where
  gliftEq _ _ (UFloat f1) (UFloat f2) = isTrue# (eqFloat# f1 f2)

instance GEq1 UInt where
  gliftEq _ _ (UInt i1) (UInt i2) = isTrue# (i1 ==# i2)

instance GEq1 UWord where
  gliftEq _ _ (UWord w1) (UWord w2) = isTrue# (eqWord# w1 w2)
#endif

---------------------------------------------------------------------------------------
-- * Ord1
---------------------------------------------------------------------------------------

-- | A sensible default 'liftCompare' implementation for 'Generic1' instances.
liftCompareDefault :: (GOrd1 (Rep1 f), Generic1 f)
                   => (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareDefault = liftCompareOptions defaultOptions

-- | Like 'liftCompareDefault', but with configurable 'Options'.
liftCompareOptions :: (GOrd1 (Rep1 f), Generic1 f)
                   => Options -> (a -> b -> Ordering) -> f a -> f b -> Ordering
liftCompareOptions opts f m n = gliftCompare opts f (from1 m) (from1 n)

class GOrd1 t where
  gliftCompare :: Options -> (a -> b -> Ordering) -> t a -> t b -> Ordering

instance Ord c => GOrd1 (K1 i c) where
  gliftCompare _ _ (K1 c) (K1 d) = compare c d

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :*: g) where
  gliftCompare opts f (a :*: b) (c :*: d) =
    gliftCompare opts f a c `mappend` gliftCompare opts f b d

instance (Ord1 f, GOrd1 g) => GOrd1 (f :.: g) where
  gliftCompare opts f (Comp1 m) (Comp1 n) = liftCompare (gliftCompare opts f) m n

instance (GOrd1 f, GOrd1 g) => GOrd1 (f :+: g) where
  gliftCompare opts f (L1 a) (L1 c) = gliftCompare opts f a c
  gliftCompare _    _ L1{}   R1{}   = LT
  gliftCompare _    _ R1{}   L1{}   = GT
  gliftCompare opts f (R1 b) (R1 d) = gliftCompare opts f b d

instance GOrd1 f => GOrd1 (M1 i c f) where
  gliftCompare opts f (M1 a) (M1 b) = gliftCompare opts f a b

instance GOrd1 U1 where
  gliftCompare _ _ U1 U1 = EQ

instance GOrd1 V1 where
  gliftCompare _ _ !_ = undefined

instance GOrd1 Par1 where
  gliftCompare _ f (Par1 a) (Par1 b) = f a b

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
-- Unboxed types
instance GOrd1 UAddr where
  gliftCompare _ _ (UAddr a1) (UAddr a2) = primCompare (eqAddr# a1 a2) (leAddr# a1 a2)

instance GOrd1 UChar where
  gliftCompare _ _ (UChar c1) (UChar c2) = primCompare (eqChar# c1 c2) (leChar# c1 c2)

instance GOrd1 UDouble where
  gliftCompare _ _ (UDouble d1) (UDouble d2) = primCompare (d1 ==## d2) (d1 <=## d2)

instance GOrd1 UFloat where
  gliftCompare _ _ (UFloat f1) (UFloat f2) = primCompare (eqFloat# f1 f2) (leFloat# f1 f2)

instance GOrd1 UInt where
  gliftCompare _ _ (UInt i1) (UInt i2) = primCompare (i1 ==# i2) (i1 <=# i2)

instance GOrd1 UWord where
  gliftCompare _ _ (UWord w1) (UWord w2) = primCompare (eqWord# w1 w2) (leWord# w1 w2)

# if __GLASGOW_HASKELL__ >= 708
primCompare :: Int# -> Int# -> Ordering
# else
primCompare :: Bool -> Bool -> Ordering
# endif
primCompare eq le = if isTrue# eq then EQ
                    else if isTrue# le then LT
                    else GT
#endif

---------------------------------------------------------------------------------------
-- * Read1
---------------------------------------------------------------------------------------

-- | A sensible default 'liftReadsPrec' implementation for 'Generic1' instances.
liftReadsPrecDefault :: (GRead1 (Rep1 f), Generic1 f)
                     => (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
liftReadsPrecDefault = liftReadsPrecOptions defaultOptions

-- | Like 'liftReadsPrecDefault', but with configurable 'Options'.
liftReadsPrecOptions :: (GRead1 (Rep1 f), Generic1 f)
                     => Options -> (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
liftReadsPrecOptions opts rp rl p =
  readPrec_to_S (fmap to1 $ parens $ gliftReadPrec opts
                                                   (readS_to_Prec rp)
                                                   (readS_to_Prec (const rl))) p

#if !(MIN_VERSION_base(4,7,0))
coerce :: a -> b
coerce = unsafeCoerce

expectP :: Lexeme -> ReadPrec ()
expectP lexeme = do
  thing <- lexP
  if thing == lexeme then return () else pfail
#endif

coerceM1 :: ReadPrec (f p) -> ReadPrec (M1 i c f p)
coerceM1 = coerce

class GRead1 f where
  gliftReadPrec :: Options -> ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)

instance GRead1 f => GRead1 (D1 d f) where
  gliftReadPrec opts rp = coerceM1 . gliftReadPrec opts rp

instance GRead1 V1 where
  gliftReadPrec _ _ _ = pfail

instance (GRead1 f, GRead1 g) => GRead1 (f :+: g) where
  gliftReadPrec opts rp rl =
    fmap L1 (gliftReadPrec opts rp rl) +++ fmap R1 (gliftReadPrec opts rp rl)

instance (Constructor c, GRead1Con f, IsNullary f) => GRead1 (C1 c f) where
  gliftReadPrec opts rp rl = coerceM1 $ case fixity of
      Prefix -> precIfNonNullary $ do
                  if conIsTuple c
                     then return ()
                     else let cn = conName c
                          in if isInfixTypeCon cn
                                then readSurround '(' (expectP (Symbol cn)) ')'
                                else expectP (Ident cn)
                  readBraces t (gliftReadPrecCon opts t rp rl)
      Infix _ m -> prec m $ gliftReadPrecCon opts t rp rl
    where
      c :: C1 c f p
      c = undefined

      x :: f p
      x = undefined

      fixity :: Fixity
      fixity = conFixity c

      precIfNonNullary :: ReadPrec a -> ReadPrec a
      precIfNonNullary = if isNullary x
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

class GRead1Con f where
  gliftReadPrecCon :: Options -> ConType -> ReadPrec a -> ReadPrec [a] -> ReadPrec (f a)

instance GRead1Con U1 where
  gliftReadPrecCon _ _ _ _ = return U1

instance GRead1Con Par1 where
  gliftReadPrecCon _ _ rp _ = coercePar1 rp
    where
      coercePar1 :: ReadPrec p -> ReadPrec (Par1 p)
      coercePar1 = coerce

instance Read c => GRead1Con (K1 i c) where
  gliftReadPrecCon _ _ _ _ = coerceK1 readPrec
    where
      coerceK1 :: ReadPrec c -> ReadPrec (K1 i c p)
      coerceK1 = coerce

instance Read1 f => GRead1Con (Rec1 f) where
  gliftReadPrecCon _ _ rp rl = coerceRec1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S rp) (readPrec_to_S rl 0)
    where
      coerceRec1 :: ReadPrec (f a) -> ReadPrec (Rec1 f a)
      coerceRec1 = coerce

instance (Selector s, GRead1Con f) => GRead1Con (S1 s f) where
  gliftReadPrecCon opts t rp rl
    | selectorName == "" = coerceM1 $ step $ gliftReadPrecCon opts t rp rl
    | otherwise          = coerceM1 $ do
                              expectP (Ident selectorName)
                              expectP (Punc "=")
                              reset $ gliftReadPrecCon opts t rp rl
    where
      selectorName :: String
      selectorName = selName (undefined :: S1 s f p)

instance (GRead1Con f, GRead1Con g) => GRead1Con (f :*: g) where
  gliftReadPrecCon opts t rp rl = do
      l <- gliftReadPrecCon opts t rp rl
      case t of
           Rec   -> expectP (Punc "=")
           Inf o -> infixPrec o
           Tup   -> expectP (Punc ",")
           Pref  -> return ()
      r <- gliftReadPrecCon opts t rp rl
      return (l :*: r)
    where
      infixPrec :: String -> ReadPrec ()
      infixPrec o = if isInfixTypeCon o
                       then expectP (Symbol o)
                       else mapM_ expectP [Punc "`", Ident o, Punc "`"]

instance (Read1 f, GRead1Con g) => GRead1Con (f :.: g) where
  gliftReadPrecCon opts t rp rl = coerceComp1 $ readS_to_Prec $
      liftReadsPrec (readPrec_to_S       (gliftReadPrecCon opts t rp rl))
                    (readPrec_to_S (list (gliftReadPrecCon opts t rp rl)) 0)
    where
      coerceComp1 :: ReadPrec (f (g a)) -> ReadPrec ((f :.: g) a)
      coerceComp1 = coerce

---------------------------------------------------------------------------------------
-- * Show1
---------------------------------------------------------------------------------------

-- | A sensible default 'liftShowsPrec' implementation for 'Generic1' instances.
liftShowsPrecDefault :: (GShow1 (Rep1 f), Generic1 f)
                     => (Int -> a -> ShowS) -> ([a] -> ShowS)
                     -> Int -> f a -> ShowS
liftShowsPrecDefault = liftShowsPrecOptions defaultOptions

-- | Like 'liftShowsPrecDefault', but with configurable 'Options'.
liftShowsPrecOptions :: (GShow1 (Rep1 f), Generic1 f)
                     => Options -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                     -> Int -> f a -> ShowS
liftShowsPrecOptions opts sp sl p = gliftShowsPrec opts sp sl p . from1

class GShow1 f where
  gliftShowsPrec :: Options -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                 -> Int -> f a -> ShowS

instance GShow1 f => GShow1 (D1 d f) where
  gliftShowsPrec opts sp sl p (M1 x) = gliftShowsPrec opts sp sl p x

instance GShow1 V1 where
  gliftShowsPrec _ _ _ _ !_ = undefined

instance (GShow1 f, GShow1 g) => GShow1 (f :+: g) where
  gliftShowsPrec opts sp sl p (L1 x) = gliftShowsPrec opts sp sl p x
  gliftShowsPrec opts sp sl p (R1 x) = gliftShowsPrec opts sp sl p x

instance (Constructor c, GShow1Con f, IsNullary f) => GShow1 (C1 c f) where
  gliftShowsPrec opts sp sl p c@(M1 x) = case fixity of
      Prefix -> showParen ( p > appPrec
                             && not (isNullary x || conIsTuple c)
                           ) $
             (if conIsTuple c
                 then id
                 else let cn = conName c
                      in showParen (isInfixTypeCon cn) (showString cn))
           . (if isNullary x || conIsTuple c
                 then id
                 else showChar ' ')
           . showBraces t (gliftShowsPrecCon opts t sp sl appPrec1 x)
      Infix _ m -> showParen (p > m) $ gliftShowsPrecCon opts t sp sl (m+1) x
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

class GShow1Con f where
  gliftShowsPrecCon :: Options -> ConType
                    -> (Int -> a -> ShowS) -> ([a] -> ShowS)
                    -> Int -> f a -> ShowS

instance GShow1Con U1 where
  gliftShowsPrecCon _ _ _ _ _ U1 = id

instance GShow1Con Par1 where
  gliftShowsPrecCon _ _ sp _  p (Par1 x) = sp p x

instance Show c => GShow1Con (K1 i c) where
  gliftShowsPrecCon _ _ _ _ p (K1 x) = showsPrec p x

instance Show1 f => GShow1Con (Rec1 f) where
  gliftShowsPrecCon _ _ sp sl p (Rec1 x) = liftShowsPrec sp sl p x

instance (Selector s, GShow1Con f) => GShow1Con (S1 s f) where
  gliftShowsPrecCon opts t sp sl p sel@(M1 x)
    | selName sel == "" =   gliftShowsPrecCon opts t sp sl p x
    | otherwise         =   showString (selName sel)
                          . showString " = "
                          . gliftShowsPrecCon opts t sp sl 0 x

instance (GShow1Con f, GShow1Con g) => GShow1Con (f :*: g) where
  gliftShowsPrecCon opts t sp sl p (a :*: b) =
    case t of
         Rec ->     gliftShowsPrecCon opts t sp sl 0 a
                  . showString ", "
                  . gliftShowsPrecCon opts t sp sl 0 b

         Inf o ->   gliftShowsPrecCon opts t sp sl p a
                  . showSpace
                  . infixOp o
                  . showSpace
                  . gliftShowsPrecCon opts t sp sl p b

         Tup ->     gliftShowsPrecCon opts t sp sl 0 a
                  . showChar ','
                  . gliftShowsPrecCon opts t sp sl 0 b

         Pref ->    gliftShowsPrecCon opts t sp sl p a
                  . showSpace
                  . gliftShowsPrecCon opts t sp sl p b
    where
      infixOp :: String -> ShowS
      infixOp o = if isInfixTypeCon o
                     then showString o
                     else showChar '`' . showString o . showChar '`'

instance (Show1 f, GShow1Con g) => GShow1Con (f :.: g) where
  gliftShowsPrecCon opts t sp sl p (Comp1 x) =
    liftShowsPrec (gliftShowsPrecCon opts t sp sl)
                  (showListWith (gliftShowsPrecCon opts t sp sl 0))
                  p x

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
instance GShow1Con UChar where
  gliftShowsPrecCon opts _ _ _ p (UChar c) =
    showsPrec (hashPrec opts p) (C# c) . oneHash opts

instance GShow1Con UDouble where
  gliftShowsPrecCon opts _ _ _ p (UDouble d) =
    showsPrec (hashPrec opts p) (D# d) . twoHash opts

instance GShow1Con UFloat where
  gliftShowsPrecCon opts _ _ _ p (UFloat f) =
    showsPrec (hashPrec opts p) (F# f) . oneHash opts

instance GShow1Con UInt where
  gliftShowsPrecCon opts _ _ _ p (UInt i) =
    showsPrec (hashPrec opts p) (I# i) . oneHash opts

instance GShow1Con UWord where
  gliftShowsPrecCon opts _ _ _ p (UWord w) =
    showsPrec (hashPrec opts p) (W# w) . twoHash opts

oneHash, twoHash :: Options -> ShowS
hashPrec         :: Options -> Int -> Int
oneHash  opts = if ghc8ShowBehavior opts then showChar   '#'  else id
twoHash  opts = if ghc8ShowBehavior opts then showString "##" else id
hashPrec opts = if ghc8ShowBehavior opts then const 0         else id
#endif

---------------------------------------------------------------------------------------
-- * Shared code
---------------------------------------------------------------------------------------

data ConType = Rec | Tup | Pref | Inf String

conIsTuple :: Constructor c => C1 c f p -> Bool
conIsTuple = isTupleString . conName

isTupleString :: String -> Bool
isTupleString ('(':',':_) = True
isTupleString _           = False

isInfixTypeCon :: String -> Bool
isInfixTypeCon (':':_) = True
isInfixTypeCon _       = False

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

#if MIN_VERSION_base(4,9,0) || defined(GENERIC_DERIVING)
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

# if __GLASGOW_HASKELL__ < 708
isTrue# :: Bool -> Bool
isTrue# = id
# endif
#endif
