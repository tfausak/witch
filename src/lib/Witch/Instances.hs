{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Witch.Instances where

import qualified Control.Exception as Exception
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.Complex as Complex
import qualified Data.Fixed as Fixed
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Typeable as Typeable
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import qualified Witch.Cast as Cast
import qualified Witch.TryCast as TryCast
import qualified Witch.TryCastException as TryCastException
import qualified Witch.Utility as Utility

-- Int8

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int8 Int.Int16 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int8 Int.Int32 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int8 Int.Int64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int8 Int where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int8 Integer where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int8 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int8 Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int8 Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int8 Word.Word64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int8 Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral' when the input is non-negative.
instance TryCast.TryCast Int.Int8 Natural.Natural where
  tryCast = Utility.maybeTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int8 Float where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int8 Double where
  cast = fromIntegral

-- Int16

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int16 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int16 Int.Int32 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int16 Int.Int64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int16 Int where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int16 Integer where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int16 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int16 Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int16 Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int16 Word.Word64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int16 Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral' when the input is non-negative.
instance TryCast.TryCast Int.Int16 Natural.Natural where
  tryCast = Utility.maybeTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int16 Float where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int16 Double where
  cast = fromIntegral

-- Int32

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int32 Int.Int64 where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int32 Integer where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Word.Word64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral' when the input is non-negative.
instance TryCast.TryCast Int.Int32 Natural.Natural where
  tryCast = Utility.maybeTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Int.Int32 Float where
  tryCast = Utility.maybeTryCast $ \s -> if -maxFloat <= s && s <= maxFloat
    then Just $ fromIntegral s
    else Nothing

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int32 Double where
  cast = fromIntegral

-- Int64

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Int.Int32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Int.Int64 Integer where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Word.Word64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int64 Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral' when the input is non-negative.
instance TryCast.TryCast Int.Int64 Natural.Natural where
  tryCast = Utility.maybeTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Int.Int64 Float where
  tryCast = Utility.maybeTryCast $ \s -> if -maxFloat <= s && s <= maxFloat
    then Just $ fromIntegral s
    else Nothing

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Int.Int64 Double where
  tryCast = Utility.maybeTryCast $ \s -> if -maxDouble <= s && s <= maxDouble
    then Just $ fromIntegral s
    else Nothing

-- Int

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Int.Int32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Int Int.Int64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Int Integer where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Word.Word64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral' when the input is non-negative.
instance TryCast.TryCast Int Natural.Natural where
  tryCast = Utility.maybeTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Int Float where
  tryCast = Utility.maybeTryCast $ \s -> if -maxFloat <= s && s <= maxFloat
    then Just $ fromIntegral s
    else Nothing

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Int Double where
  tryCast = Utility.maybeTryCast $ \s ->
    if (toInteger (maxBound :: Int) <= maxDouble)
        || (-maxDouble <= s && s <= maxDouble)
      then Just $ fromIntegral s
      else Nothing

-- Integer

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Int.Int32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Int.Int64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Word.Word64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Integer Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromInteger' when the input is non-negative.
instance TryCast.TryCast Integer Natural.Natural where
  -- This should use @maybeTryCast fromNonNegativeIntegral@, but that causes a
  -- bug in GHC 9.0.1. By inlining @fromNonNegativeIntegral@ and replacing
  -- @fromIntegral@ with @fromInteger@, we can work around the bug.
  -- https://mail.haskell.org/pipermail/haskell-cafe/2021-March/133540.html
  tryCast =
    Utility.maybeTryCast $ \s -> if s < 0 then Nothing else Just $ fromInteger s

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Integer Float where
  tryCast = Utility.maybeTryCast $ \s -> if -maxFloat <= s && s <= maxFloat
    then Just $ fromIntegral s
    else Nothing

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Integer Double where
  tryCast = Utility.maybeTryCast $ \s -> if -maxDouble <= s && s <= maxDouble
    then Just $ fromIntegral s
    else Nothing

-- Word8

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Word.Word16 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Word.Word32 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Word.Word64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Word where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Natural.Natural where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word8 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Int.Int16 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Int.Int32 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Int.Int64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Int where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Integer where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Float where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word8 Double where
  cast = fromIntegral

-- Word16

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word16 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Word.Word32 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Word.Word64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Word where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Natural.Natural where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word16 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word16 Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Int.Int32 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Int.Int64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Int where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Integer where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Float where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word16 Double where
  cast = fromIntegral

-- Word32

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word32 Word.Word64 where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word32 Natural.Natural where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Int.Int32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word32 Int.Int64 where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word32 Integer where
  cast = fromIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Word.Word32 Float where
  tryCast = Utility.maybeTryCast
    $ \s -> if s <= maxFloat then Just $ fromIntegral s else Nothing

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word32 Double where
  cast = fromIntegral

-- Word64

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word64 Natural.Natural where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Int.Int32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Int.Int64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word64 Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word.Word64 Integer where
  cast = fromIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Word.Word64 Float where
  tryCast = Utility.maybeTryCast
    $ \s -> if s <= maxFloat then Just $ fromIntegral s else Nothing

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Word.Word64 Double where
  tryCast = Utility.maybeTryCast
    $ \s -> if s <= maxDouble then Just $ fromIntegral s else Nothing

-- Word

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word Word.Word64 where
  cast = fromIntegral

-- | Uses 'fromIntegral'.
instance Cast.Cast Word Natural.Natural where
  cast = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Int.Int32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Int.Int64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Word Integer where
  cast = fromIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Word Float where
  tryCast = Utility.maybeTryCast
    $ \s -> if s <= maxFloat then Just $ fromIntegral s else Nothing

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Word Double where
  tryCast = Utility.maybeTryCast $ \s ->
    if (toInteger (maxBound :: Word) <= maxDouble) || (s <= maxDouble)
      then Just $ fromIntegral s
      else Nothing

-- Natural

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Word.Word32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Word.Word64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Int.Int32 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Int.Int64 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Natural.Natural Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance Cast.Cast Natural.Natural Integer where
  cast = fromIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Natural.Natural Float where
  tryCast = Utility.maybeTryCast
    $ \s -> if s <= maxFloat then Just $ fromIntegral s else Nothing

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Natural.Natural Double where
  tryCast = Utility.maybeTryCast
    $ \s -> if s <= maxDouble then Just $ fromIntegral s else Nothing

-- Float

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Int.Int8 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Int.Int16 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Int.Int32 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Int.Int64 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Int where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Rational' when the input is between -16,777,215 and
-- 16,777,215 inclusive.
instance TryCast.TryCast Float Integer where
  tryCast s = case Utility.tryVia @Rational s of
    Left e -> Left e
    Right t
      | t < -maxFloat -> Left . TryCastException.TryCastException s . Just $ Exception.toException Exception.Underflow
      | t > maxFloat -> Left . TryCastException.TryCastException s . Just $ Exception.toException Exception.Overflow
      | otherwise -> Right t

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Word.Word8 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Word.Word16 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Word.Word32 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Word.Word64 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Word where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Float Natural.Natural where
  tryCast = Utility.tryVia @Integer

-- | Uses 'toRational' when the input is not NaN or infinity.
instance TryCast.TryCast Float Rational where
  tryCast = Utility.maybeTryCast
    $ \s -> if isNaN s || isInfinite s then Nothing else Just $ toRational s

-- | Uses 'realToFrac'.
instance Cast.Cast Float Double where
  cast = realToFrac

-- Double

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Int.Int8 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Int.Int16 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Int.Int32 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Int.Int64 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Int where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Rational' when the input is between -9,007,199,254,740,991
-- and 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Double Integer where
  tryCast s = case Utility.tryVia @Rational s of
    Left e -> Left e
    Right t
      | t < -maxDouble -> Left . TryCastException.TryCastException s . Just $ Exception.toException Exception.Underflow
      | t > maxDouble -> Left . TryCastException.TryCastException s . Just $ Exception.toException Exception.Overflow
      | otherwise -> Right t

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Word.Word8 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Word.Word16 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Word.Word32 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Word.Word64 where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Word where
  tryCast = Utility.tryVia @Integer

-- | Converts via 'Integer'.
instance TryCast.TryCast Double Natural.Natural where
  tryCast = Utility.tryVia @Integer

-- | Uses 'toRational' when the input is not NaN or infinity.
instance TryCast.TryCast Double Rational where
  tryCast = Utility.maybeTryCast
    $ \s -> if isNaN s || isInfinite s then Nothing else Just $ toRational s

-- | Uses 'realToFrac'. This necessarily loses some precision.
instance Cast.Cast Double Float where
  cast = realToFrac

-- Ratio

-- | Uses '(Ratio.%)' with a denominator of 1.
instance Integral a => Cast.Cast a (Ratio.Ratio a) where
  cast = (Ratio.% 1)

-- | Uses 'Ratio.numerator' when the denominator is 1.
instance (Eq a, Num a) => TryCast.TryCast (Ratio.Ratio a) a where
  tryCast = Utility.maybeTryCast $ \s ->
    if Ratio.denominator s == 1 then Just $ Ratio.numerator s else Nothing

-- | Uses 'fromRational'. This necessarily loses some precision.
instance Cast.Cast Rational Float where
  cast = fromRational

-- | Uses 'fromRational'. This necessarily loses some precision.
instance Cast.Cast Rational Double where
  cast = fromRational

-- Fixed

-- | Uses 'Fixed.MkFixed'. This means @cast 2 :: Centi@ is @0.02@ rather than
-- @2.00@.
instance Cast.Cast Integer (Fixed.Fixed a) where
  cast = Fixed.MkFixed

-- | Uses 'Fixed.MkFixed'. This means @cast (3.00 :: Centi)@ is @300@ rather
-- than @3@.
instance Cast.Cast (Fixed.Fixed a) Integer where
  cast (Fixed.MkFixed t) = t

-- Complex

-- | Uses '(Complex.:+)' with an imaginary part of 0.
instance Num a => Cast.Cast a (Complex.Complex a) where
  cast = (Complex.:+ 0)

-- | Uses 'Complex.realPart' when the imaginary part is 0.
instance (Eq a, Num a) => TryCast.TryCast (Complex.Complex a) a where
  tryCast = Utility.maybeTryCast $ \s ->
    if Complex.imagPart s == 0 then Just $ Complex.realPart s else Nothing

-- NonEmpty

-- | Uses 'NonEmpty.nonEmpty'.
instance TryCast.TryCast [a] (NonEmpty.NonEmpty a) where
  tryCast = Utility.maybeTryCast NonEmpty.nonEmpty

-- | Uses 'NonEmpty.toList'.
instance Cast.Cast (NonEmpty.NonEmpty a) [a] where
  cast = NonEmpty.toList

-- Set

-- | Uses 'Set.fromList'.
instance Ord a => Cast.Cast [a] (Set.Set a) where
  cast = Set.fromList

-- | Uses 'Set.toAscList'.
instance Cast.Cast (Set.Set a) [a] where
  cast = Set.toAscList

-- IntSet

-- | Uses 'IntSet.fromList'.
instance Cast.Cast [Int] IntSet.IntSet where
  cast = IntSet.fromList

-- | Uses 'IntSet.toAscList'.
instance Cast.Cast IntSet.IntSet [Int] where
  cast = IntSet.toAscList

-- Map

-- | Uses 'Map.fromList'. If there are duplicate keys, later values will
-- overwrite earlier ones.
instance Ord k => Cast.Cast [(k, v)] (Map.Map k v) where
  cast = Map.fromList

-- | Uses 'Map.toAscList'.
instance Cast.Cast (Map.Map k v) [(k, v)] where
  cast = Map.toAscList

-- IntMap

-- | Uses 'IntMap.fromList'. If there are duplicate keys, later values will
-- overwrite earlier ones.
instance Cast.Cast [(Int, v)] (IntMap.IntMap v) where
  cast = IntMap.fromList

-- | Uses 'IntMap.toAscList'.
instance Cast.Cast (IntMap.IntMap v) [(Int, v)] where
  cast = IntMap.toAscList

-- Seq

-- | Uses 'Seq.fromList'.
instance Cast.Cast [a] (Seq.Seq a) where
  cast = Seq.fromList

-- | Uses 'Foldable.toList'.
instance Cast.Cast (Seq.Seq a) [a] where
  cast = Foldable.toList

-- ByteString

-- | Uses 'ByteString.pack'.
instance Cast.Cast [Word.Word8] ByteString.ByteString where
  cast = ByteString.pack

-- | Uses 'ByteString.unpack'.
instance Cast.Cast ByteString.ByteString [Word.Word8] where
  cast = ByteString.unpack

-- | Uses 'LazyByteString.fromStrict'.
instance Cast.Cast ByteString.ByteString LazyByteString.ByteString where
  cast = LazyByteString.fromStrict

-- | Uses 'ShortByteString.toShort'.
instance Cast.Cast ByteString.ByteString ShortByteString.ShortByteString where
  cast = ShortByteString.toShort

-- | Uses 'Text.decodeUtf8''.
instance TryCast.TryCast ByteString.ByteString Text.Text where
  tryCast s = case Text.decodeUtf8' s of
    Left e -> Left . TryCastException.TryCastException s . Just $ Exception.toException e
    Right t -> Right t

-- LazyByteString

-- | Uses 'LazyByteString.pack'.
instance Cast.Cast [Word.Word8] LazyByteString.ByteString where
  cast = LazyByteString.pack

-- | Uses 'LazyByteString.unpack'.
instance Cast.Cast LazyByteString.ByteString [Word.Word8] where
  cast = LazyByteString.unpack

-- | Uses 'LazyByteString.toStrict'.
instance Cast.Cast LazyByteString.ByteString ByteString.ByteString where
  cast = LazyByteString.toStrict

-- | Uses 'LazyText.decodeUtf8''.
instance TryCast.TryCast LazyByteString.ByteString LazyText.Text where
  tryCast s = case LazyText.decodeUtf8' s of
    Left e -> Left . TryCastException.TryCastException s . Just $ Exception.toException e
    Right t -> Right t

-- ShortByteString

-- | Uses 'ShortByteString.pack'.
instance Cast.Cast [Word.Word8] ShortByteString.ShortByteString where
  cast = ShortByteString.pack

-- | Uses 'ShortByteString.unpack'.
instance Cast.Cast ShortByteString.ShortByteString [Word.Word8] where
  cast = ShortByteString.unpack

-- | Uses 'ShortByteString.fromShort'.
instance Cast.Cast ShortByteString.ShortByteString ByteString.ByteString where
  cast = ShortByteString.fromShort

-- Text

-- | Uses 'Text.pack'. Some 'Char' values cannot be represented in 'Text.Text'
-- and will be replaced with @'\\xFFFD'@.
instance Cast.Cast String Text.Text where
  cast = Text.pack

-- | Uses 'Text.unpack'.
instance Cast.Cast Text.Text String where
  cast = Text.unpack

-- | Uses 'LazyText.fromStrict'.
instance Cast.Cast Text.Text LazyText.Text where
  cast = LazyText.fromStrict

-- | Uses 'Text.encodeUtf8'.
instance Cast.Cast Text.Text ByteString.ByteString where
  cast = Text.encodeUtf8

-- LazyText

-- | Uses 'LazyText.pack'. Some 'Char' values cannot be represented in
-- 'LazyText.Text' and will be replaced with @'\\xFFFD'@.
instance Cast.Cast String LazyText.Text where
  cast = LazyText.pack

-- | Uses 'LazyText.unpack'.
instance Cast.Cast LazyText.Text String where
  cast = LazyText.unpack

-- | Uses 'LazyText.toStrict'.
instance Cast.Cast LazyText.Text Text.Text where
  cast = LazyText.toStrict

-- | Uses 'LazyText.encodeUtf8'.
instance Cast.Cast LazyText.Text LazyByteString.ByteString where
  cast = LazyText.encodeUtf8

-- TryCastException

instance Cast.Cast (TryCastException.TryCastException s t0) (TryCastException.TryCastException s t1)

instance
  ( Show s
  , Typeable.Typeable s
  , Typeable.Typeable t
  ) => Cast.Cast (TryCastException.TryCastException s t) String where
  cast = show

instance
  ( Show s
  , Typeable.Typeable s
  , Typeable.Typeable t
  ) => Cast.Cast (TryCastException.TryCastException s t) Text.Text where
  cast = Utility.via @String

instance
  ( Show s
  , Typeable.Typeable s
  , Typeable.Typeable t
  ) => Cast.Cast (TryCastException.TryCastException s t) LazyText.Text where
  cast = Utility.via @String

fromNonNegativeIntegral :: (Integral s, Num t) => s -> Maybe t
fromNonNegativeIntegral x = if x < 0 then Nothing else Just $ fromIntegral x

-- | The maximum integral value that can be unambiguously represented as a
-- 'Float'. Equal to 16,777,215.
maxFloat :: Num a => a
maxFloat = 16777215

-- | The maximum integral value that can be unambiguously represented as a
-- 'Double'. Equal to 9,007,199,254,740,991.
maxDouble :: Num a => a
maxDouble = 9007199254740991
