{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.Instances where

import qualified Data.Bits as Bits
import qualified Data.Complex as Complex
import qualified Data.Fixed as Fixed
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Ratio as Ratio
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import qualified Witch.Cast as Cast
import qualified Witch.TryCast as TryCast
import qualified Witch.TryCastException as TryCastException

-- NonEmpty

instance TryCast.TryCast [a] (NonEmpty.NonEmpty a) where
  tryCast = maybeTryCast NonEmpty.nonEmpty

instance Cast.Cast (NonEmpty.NonEmpty a) [a] where
  cast = NonEmpty.toList

-- Int8

instance Cast.Cast Int.Int8 Int.Int16 where
  cast = fromIntegral

instance Cast.Cast Int.Int8 Int.Int32 where
  cast = fromIntegral

instance Cast.Cast Int.Int8 Int.Int64 where
  cast = fromIntegral

instance Cast.Cast Int.Int8 Int where
  cast = fromIntegral

instance Cast.Cast Int.Int8 Integer where
  cast = fromIntegral

instance TryCast.TryCast Int.Int8 Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int8 Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int8 Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int8 Word.Word64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int8 Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int8 Natural.Natural where
  tryCast = maybeTryCast fromNonNegativeIntegral

instance Cast.Cast Int.Int8 Float where
  cast = fromIntegral

instance Cast.Cast Int.Int8 Double where
  cast = fromIntegral

-- Int16

instance TryCast.TryCast Int.Int16 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Int.Int16 Int.Int32 where
  cast = fromIntegral

instance Cast.Cast Int.Int16 Int.Int64 where
  cast = fromIntegral

instance Cast.Cast Int.Int16 Int where
  cast = fromIntegral

instance Cast.Cast Int.Int16 Integer where
  cast = fromIntegral

instance TryCast.TryCast Int.Int16 Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int16 Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int16 Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int16 Word.Word64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int16 Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int16 Natural.Natural where
  tryCast = maybeTryCast fromNonNegativeIntegral

instance Cast.Cast Int.Int16 Float where
  cast = fromIntegral

instance Cast.Cast Int.Int16 Double where
  cast = fromIntegral

-- Int32

instance TryCast.TryCast Int.Int32 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int32 Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Int.Int32 Int.Int64 where
  cast = fromIntegral

instance TryCast.TryCast Int.Int32 Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Int.Int32 Integer where
  cast = fromIntegral

instance TryCast.TryCast Int.Int32 Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int32 Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int32 Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int32 Word.Word64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int32 Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int32 Natural.Natural where
  tryCast = maybeTryCast fromNonNegativeIntegral

instance TryCast.TryCast Int.Int32 Float where
  tryCast = maybeTryCast $ \s ->
    if -16777216 <= s && s <= 16777216 then Just $ fromIntegral s else Nothing

instance Cast.Cast Int.Int32 Double where
  cast = fromIntegral

-- Int64

instance TryCast.TryCast Int.Int64 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Int.Int64 Integer where
  cast = fromIntegral

instance TryCast.TryCast Int.Int64 Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Word.Word64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int.Int64 Natural.Natural where
  tryCast = maybeTryCast fromNonNegativeIntegral

instance TryCast.TryCast Int.Int64 Float where
  tryCast = maybeTryCast $ \s ->
    if -16777216 <= s && s <= 16777216 then Just $ fromIntegral s else Nothing

instance TryCast.TryCast Int.Int64 Double where
  tryCast = maybeTryCast $ \s ->
    if -9007199254740992 <= s && s <= 9007199254740992
      then Just $ fromIntegral s
      else Nothing

-- Int

instance TryCast.TryCast Int Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Int Int.Int64 where
  cast = fromIntegral

instance Cast.Cast Int Integer where
  cast = fromIntegral

instance TryCast.TryCast Int Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int Word.Word64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Int Natural.Natural where
  tryCast = maybeTryCast fromNonNegativeIntegral

instance TryCast.TryCast Int Float where
  tryCast = maybeTryCast $ \s ->
    if -16777216 <= s && s <= 16777216 then Just $ fromIntegral s else Nothing

instance TryCast.TryCast Int Double where
  tryCast = maybeTryCast $ \s ->
    if toInteger (maxBound :: Int) <= 9007199254740992
      then Just $ fromIntegral s
      else if -9007199254740992 <= s && s <= 9007199254740992
        then Just $ fromIntegral s
        else Nothing

-- Integer

instance TryCast.TryCast Integer Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Int.Int64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Word.Word64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Integer Natural.Natural where
  -- This should use @maybeTryCast fromNonNegativeIntegral@, but that causes a
  -- bug in GHC 9.0.1. By inlining @fromNonNegativeIntegral@ and replacing
  -- @fromIntegral@ with @fromInteger@, we can work around the bug.
  -- https://mail.haskell.org/pipermail/haskell-cafe/2021-March/133540.html
  tryCast =
    maybeTryCast $ \s -> if s < 0 then Nothing else Just $ fromInteger s

instance TryCast.TryCast Integer Float where
  tryCast = maybeTryCast $ \s ->
    if -16777216 <= s && s <= 16777216 then Just $ fromIntegral s else Nothing

instance TryCast.TryCast Integer Double where
  tryCast = maybeTryCast $ \s ->
    if -9007199254740992 <= s && s <= 9007199254740992
      then Just $ fromIntegral s
      else Nothing

-- Word8

instance Cast.Cast Word.Word8 Word.Word16 where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Word.Word32 where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Word.Word64 where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Word where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Natural.Natural where
  cast = fromIntegral

instance TryCast.TryCast Word.Word8 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word8 Int.Int16 where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Int.Int32 where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Int.Int64 where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Int where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Integer where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Float where
  cast = fromIntegral

instance Cast.Cast Word.Word8 Double where
  cast = fromIntegral

-- Word16

instance TryCast.TryCast Word.Word16 Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word16 Word.Word32 where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Word.Word64 where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Word where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Natural.Natural where
  cast = fromIntegral

instance TryCast.TryCast Word.Word16 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word16 Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word16 Int.Int32 where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Int.Int64 where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Int where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Integer where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Float where
  cast = fromIntegral

instance Cast.Cast Word.Word16 Double where
  cast = fromIntegral

-- Word32

instance TryCast.TryCast Word.Word32 Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word32 Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word32 Word.Word64 where
  cast = fromIntegral

instance TryCast.TryCast Word.Word32 Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word32 Natural.Natural where
  cast = fromIntegral

instance TryCast.TryCast Word.Word32 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word32 Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word32 Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word32 Int.Int64 where
  cast = fromIntegral

instance TryCast.TryCast Word.Word32 Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word32 Integer where
  cast = fromIntegral

instance TryCast.TryCast Word.Word32 Float where
  tryCast = maybeTryCast $ \s ->
    if s <= 16777216 then Just $ fromIntegral s else Nothing

instance Cast.Cast Word.Word32 Double where
  cast = fromIntegral

-- Word64

instance TryCast.TryCast Word.Word64 Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word64 Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word64 Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word64 Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word64 Natural.Natural where
  cast = fromIntegral

instance TryCast.TryCast Word.Word64 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word64 Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word64 Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word64 Int.Int64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word.Word64 Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word.Word64 Integer where
  cast = fromIntegral

instance TryCast.TryCast Word.Word64 Float where
  tryCast = maybeTryCast $ \s ->
    if s <= 16777216 then Just $ fromIntegral s else Nothing

instance TryCast.TryCast Word.Word64 Double where
  tryCast = maybeTryCast $ \s ->
    if s <= 9007199254740992 then Just $ fromIntegral s else Nothing

-- Word

instance TryCast.TryCast Word Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word Word.Word64 where
  cast = fromIntegral

instance Cast.Cast Word Natural.Natural where
  cast = fromIntegral

instance TryCast.TryCast Word Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word Int.Int64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Word Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Word Integer where
  cast = fromIntegral

instance TryCast.TryCast Word Float where
  tryCast = maybeTryCast $ \s ->
    if s <= 16777216 then Just $ fromIntegral s else Nothing

instance TryCast.TryCast Word Double where
  tryCast = maybeTryCast $ \s ->
    if toInteger (maxBound :: Word) <= 9007199254740992
      then Just $ fromIntegral s
      else if s <= 9007199254740992 then Just $ fromIntegral s else Nothing

-- Natural

instance TryCast.TryCast Natural.Natural Word.Word8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Word.Word16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Word.Word32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Word.Word64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Word where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Int.Int64 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast.TryCast Natural.Natural Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance Cast.Cast Natural.Natural Integer where
  cast = fromIntegral

instance TryCast.TryCast Natural.Natural Float where
  tryCast = maybeTryCast $ \s ->
    if s <= 16777216 then Just $ fromIntegral s else Nothing

instance TryCast.TryCast Natural.Natural Double where
  tryCast = maybeTryCast $ \s ->
    if s <= 9007199254740992 then Just $ fromIntegral s else Nothing

-- Ratio

instance Integral a => Cast.Cast a (Ratio.Ratio a) where
  cast = (Ratio.% 1)

instance (Eq a, Num a) => TryCast.TryCast (Ratio.Ratio a) a where
  tryCast = maybeTryCast $ \s ->
    if Ratio.denominator s == 1 then Just $ Ratio.numerator s else Nothing

-- Fixed

instance Cast.Cast Integer (Fixed.Fixed a) where
  cast = Fixed.MkFixed

instance Cast.Cast (Fixed.Fixed a) Integer where
  cast (Fixed.MkFixed t) = t

-- Complex

instance Num a => Cast.Cast a (Complex.Complex a) where
  cast = (Complex.:+ 0)

instance (Eq a, Num a) => TryCast.TryCast (Complex.Complex a) a where
  tryCast = maybeTryCast $ \s ->
    if Complex.imagPart s == 0 then Just $ Complex.realPart s else Nothing

fromNonNegativeIntegral :: (Integral s, Num t) => s -> Maybe t
fromNonNegativeIntegral x = if x < 0 then Nothing else Just $ fromIntegral x

maybeTryCast
  :: (s -> Maybe t) -> s -> Either (TryCastException.TryCastException s t) t
maybeTryCast f s = case f s of
  Nothing -> Left $ TryCastException.TryCastException s
  Just t -> Right t
