{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified GHC.Float as Float
import qualified Numeric.Natural as Natural
import qualified Witch.From as From
import qualified Witch.TryCast as TryCast
import qualified Witch.TryCastException as TryCastException
import qualified Witch.Utility as Utility

-- Int8

-- | Uses 'fromIntegral'.
instance From.From Int.Int8 Int.Int16 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int8 Int.Int32 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int8 Int.Int64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int8 Int where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int8 Integer where
  from = fromIntegral

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

-- | Uses 'fromIntegral' when the input is not negative.
instance TryCast.TryCast Int.Int8 Natural.Natural where
  tryCast = Utility.eitherTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int8 Float where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int8 Double where
  from = fromIntegral

-- Int16

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int16 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Int.Int16 Int.Int32 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int16 Int.Int64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int16 Int where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int16 Integer where
  from = fromIntegral

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

-- | Uses 'fromIntegral' when the input is not negative.
instance TryCast.TryCast Int.Int16 Natural.Natural where
  tryCast = Utility.eitherTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int16 Float where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int.Int16 Double where
  from = fromIntegral

-- Int32

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Int.Int32 Int.Int64 where
  from = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Int.Int32 Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Int.Int32 Integer where
  from = fromIntegral

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

-- | Uses 'fromIntegral' when the input is not negative.
instance TryCast.TryCast Int.Int32 Natural.Natural where
  tryCast = Utility.eitherTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Int.Int32 Float where
  tryCast = Utility.eitherTryCast $ \s -> if s < -maxFloat
    then Left Exception.Underflow
    else if s > maxFloat
      then Left Exception.Overflow
      else Right $ fromIntegral s

-- | Uses 'fromIntegral'.
instance From.From Int.Int32 Double where
  from = fromIntegral

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
instance From.From Int.Int64 Integer where
  from = fromIntegral

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

-- | Uses 'fromIntegral' when the input is not negative.
instance TryCast.TryCast Int.Int64 Natural.Natural where
  tryCast = Utility.eitherTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Int.Int64 Float where
  tryCast = Utility.eitherTryCast $ \s -> if s < -maxFloat
    then Left Exception.Underflow
    else if s > maxFloat
      then Left Exception.Overflow
      else Right $ fromIntegral s

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Int.Int64 Double where
  tryCast = Utility.eitherTryCast $ \s -> if s < -maxDouble
    then Left Exception.Underflow
    else if s > maxDouble
      then Left Exception.Overflow
      else Right $ fromIntegral s

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
instance From.From Int Int.Int64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Int Integer where
  from = fromIntegral

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

-- | Uses 'fromIntegral' when the input is not negative.
instance TryCast.TryCast Int Natural.Natural where
  tryCast = Utility.eitherTryCast fromNonNegativeIntegral

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Int Float where
  tryCast = Utility.eitherTryCast $ \s -> if s < -maxFloat
    then Left Exception.Underflow
    else if s > maxFloat
      then Left Exception.Overflow
      else Right $ fromIntegral s

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Int Double where
  tryCast = Utility.eitherTryCast $ \s ->
    if toInteger (maxBound :: Int) <= maxDouble
      then Right $ fromIntegral s
      else if s < -maxDouble
        then Left Exception.Underflow
        else if s > maxDouble
          then Left Exception.Overflow
          else Right $ fromIntegral s

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

-- | Uses 'fromInteger' when the input is not negative.
instance TryCast.TryCast Integer Natural.Natural where
  -- This should use @eitherTryCast fromNonNegativeIntegral@, but that causes
  -- a bug in GHC 9.0.1. By inlining @fromNonNegativeIntegral@ and replacing
  -- @fromIntegral@ with @fromInteger@, we can work around the bug.
  -- https://mail.haskell.org/pipermail/haskell-cafe/2021-March/133540.html
  tryCast = Utility.eitherTryCast
    $ \s -> if s < 0 then Left Exception.Underflow else Right $ fromInteger s

-- | Uses 'fromIntegral' when the input is between -16,777,215 and 16,777,215
-- inclusive.
instance TryCast.TryCast Integer Float where
  tryCast = Utility.eitherTryCast $ \s -> if s < -maxFloat
    then Left Exception.Underflow
    else if s > maxFloat
      then Left Exception.Overflow
      else Right $ fromIntegral s

-- | Uses 'fromIntegral' when the input is between -9,007,199,254,740,991 and
-- 9,007,199,254,740,991 inclusive.
instance TryCast.TryCast Integer Double where
  tryCast = Utility.eitherTryCast $ \s -> if s < -maxDouble
    then Left Exception.Underflow
    else if s > maxDouble
      then Left Exception.Overflow
      else Right $ fromIntegral s

-- Word8

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Word.Word16 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Word.Word32 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Word.Word64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Word where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Natural.Natural where
  from = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word8 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Int.Int16 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Int.Int32 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Int.Int64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Int where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Integer where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Float where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word8 Double where
  from = fromIntegral

-- Word16

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word16 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Word.Word32 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Word.Word64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Word where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Natural.Natural where
  from = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word16 Int.Int8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word16 Int.Int16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Int.Int32 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Int.Int64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Int where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Integer where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Float where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word.Word16 Double where
  from = fromIntegral

-- Word32

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Word.Word8 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Word.Word16 where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Word.Word32 Word.Word64 where
  from = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Word where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Word.Word32 Natural.Natural where
  from = fromIntegral

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
instance From.From Word.Word32 Int.Int64 where
  from = fromIntegral

-- | Uses 'Bits.toIntegralSized'.
instance TryCast.TryCast Word.Word32 Int where
  tryCast = Utility.maybeTryCast Bits.toIntegralSized

-- | Uses 'fromIntegral'.
instance From.From Word.Word32 Integer where
  from = fromIntegral

-- | Uses 'fromIntegral' when the input is less than or equal to 16,777,215.
instance TryCast.TryCast Word.Word32 Float where
  tryCast = Utility.eitherTryCast $ \s ->
    if s <= maxFloat then Right $ fromIntegral s else Left Exception.Overflow

-- | Uses 'fromIntegral'.
instance From.From Word.Word32 Double where
  from = fromIntegral

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
instance From.From Word.Word64 Natural.Natural where
  from = fromIntegral

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
instance From.From Word.Word64 Integer where
  from = fromIntegral

-- | Uses 'fromIntegral' when the input is less than or equal to 16,777,215.
instance TryCast.TryCast Word.Word64 Float where
  tryCast = Utility.eitherTryCast $ \s ->
    if s <= maxFloat then Right $ fromIntegral s else Left Exception.Overflow

-- | Uses 'fromIntegral' when the input is less than or equal to
-- 9,007,199,254,740,991.
instance TryCast.TryCast Word.Word64 Double where
  tryCast = Utility.eitherTryCast $ \s -> if s <= maxDouble
    then Right $ fromIntegral s
    else Left Exception.Overflow

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
instance From.From Word Word.Word64 where
  from = fromIntegral

-- | Uses 'fromIntegral'.
instance From.From Word Natural.Natural where
  from = fromIntegral

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
instance From.From Word Integer where
  from = fromIntegral

-- | Uses 'fromIntegral' when the input is less than or equal to 16,777,215.
instance TryCast.TryCast Word Float where
  tryCast = Utility.eitherTryCast $ \s ->
    if s <= maxFloat then Right $ fromIntegral s else Left Exception.Overflow

-- | Uses 'fromIntegral' when the input is less than or equal to
-- 9,007,199,254,740,991.
instance TryCast.TryCast Word Double where
  tryCast = Utility.eitherTryCast $ \s ->
    if (toInteger (maxBound :: Word) <= maxDouble) || (s <= maxDouble)
      then Right $ fromIntegral s
      else Left Exception.Overflow

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
instance From.From Natural.Natural Integer where
  from = fromIntegral

-- | Uses 'fromIntegral' when the input is less than or equal to 16,777,215.
instance TryCast.TryCast Natural.Natural Float where
  tryCast = Utility.eitherTryCast $ \s ->
    if s <= maxFloat then Right $ fromIntegral s else Left Exception.Overflow

-- | Uses 'fromIntegral' when the input is less than or equal to
-- 9,007,199,254,740,991.
instance TryCast.TryCast Natural.Natural Double where
  tryCast = Utility.eitherTryCast $ \s -> if s <= maxDouble
    then Right $ fromIntegral s
    else Left Exception.Overflow

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
  tryCast = Utility.eitherTryCast $ \s -> case Utility.tryVia @Rational s of
    Left e -> Left $ Exception.toException e
    Right t
      | t < -maxFloat -> Left $ Exception.toException Exception.Underflow
      | t > maxFloat -> Left $ Exception.toException Exception.Overflow
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
  tryCast = Utility.eitherTryCast $ \s -> if isNaN s
    then Left Exception.LossOfPrecision
    else if isInfinite s
      then if s > 0 then Left Exception.Overflow else Left Exception.Underflow
      else Right $ toRational s

-- | Uses 'Float.float2Double'.
instance From.From Float Double where
  from = Float.float2Double

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
  tryCast = Utility.eitherTryCast $ \s -> case Utility.tryVia @Rational s of
    Left e -> Left $ Exception.toException e
    Right t
      | t < -maxDouble -> Left $ Exception.toException Exception.Underflow
      | t > maxDouble -> Left $ Exception.toException Exception.Overflow
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
  tryCast = Utility.eitherTryCast $ \s -> if isNaN s
    then Left Exception.LossOfPrecision
    else if isInfinite s
      then if s > 0 then Left Exception.Overflow else Left Exception.Underflow
      else Right $ toRational s

-- | Uses 'Float.double2Float'. This necessarily loses some precision.
instance From.From Double Float where
  from = Float.double2Float

-- Ratio

-- | Uses '(Ratio.%)' with a denominator of 1.
instance Integral a => From.From a (Ratio.Ratio a) where
  from = (Ratio.% 1)

-- | Uses 'Ratio.numerator' when the denominator is 1.
instance (Eq a, Num a) => TryCast.TryCast (Ratio.Ratio a) a where
  tryCast = Utility.eitherTryCast $ \s -> if Ratio.denominator s == 1
    then Right $ Ratio.numerator s
    else Left Exception.LossOfPrecision

-- | Uses 'fromRational'. This necessarily loses some precision.
instance From.From Rational Float where
  from = fromRational

-- | Uses 'fromRational'. This necessarily loses some precision.
instance From.From Rational Double where
  from = fromRational

-- Fixed

-- | Uses 'Fixed.MkFixed'. This means @from 2 :: Centi@ is @0.02@ rather than
-- @2.00@.
instance From.From Integer (Fixed.Fixed a) where
  from = Fixed.MkFixed

-- | Uses 'Fixed.MkFixed'. This means @from (3.00 :: Centi)@ is @300@ rather
-- than @3@.
instance From.From (Fixed.Fixed a) Integer where
  from (Fixed.MkFixed t) = t

-- Complex

-- | Uses '(Complex.:+)' with an imaginary part of 0.
instance Num a => From.From a (Complex.Complex a) where
  from = (Complex.:+ 0)

-- | Uses 'Complex.realPart' when the imaginary part is 0.
instance (Eq a, Num a) => TryCast.TryCast (Complex.Complex a) a where
  tryCast = Utility.eitherTryCast $ \s -> if Complex.imagPart s == 0
    then Right $ Complex.realPart s
    else Left Exception.LossOfPrecision

-- NonEmpty

-- | Uses 'NonEmpty.nonEmpty'.
instance TryCast.TryCast [a] (NonEmpty.NonEmpty a) where
  tryCast = Utility.maybeTryCast NonEmpty.nonEmpty

-- | Uses 'NonEmpty.toList'.
instance From.From (NonEmpty.NonEmpty a) [a] where
  from = NonEmpty.toList

-- Set

-- | Uses 'Set.fromList'.
instance Ord a => From.From [a] (Set.Set a) where
  from = Set.fromList

-- | Uses 'Set.toAscList'.
instance From.From (Set.Set a) [a] where
  from = Set.toAscList

-- IntSet

-- | Uses 'IntSet.fromList'.
instance From.From [Int] IntSet.IntSet where
  from = IntSet.fromList

-- | Uses 'IntSet.toAscList'.
instance From.From IntSet.IntSet [Int] where
  from = IntSet.toAscList

-- Map

-- | Uses 'Map.fromList'. If there are duplicate keys, later values will
-- overwrite earlier ones.
instance Ord k => From.From [(k, v)] (Map.Map k v) where
  from = Map.fromList

-- | Uses 'Map.toAscList'.
instance From.From (Map.Map k v) [(k, v)] where
  from = Map.toAscList

-- IntMap

-- | Uses 'IntMap.fromList'. If there are duplicate keys, later values will
-- overwrite earlier ones.
instance From.From [(Int, v)] (IntMap.IntMap v) where
  from = IntMap.fromList

-- | Uses 'IntMap.toAscList'.
instance From.From (IntMap.IntMap v) [(Int, v)] where
  from = IntMap.toAscList

-- Seq

-- | Uses 'Seq.fromList'.
instance From.From [a] (Seq.Seq a) where
  from = Seq.fromList

-- | Uses 'Foldable.toList'.
instance From.From (Seq.Seq a) [a] where
  from = Foldable.toList

-- ByteString

-- | Uses 'ByteString.pack'.
instance From.From [Word.Word8] ByteString.ByteString where
  from = ByteString.pack

-- | Uses 'ByteString.unpack'.
instance From.From ByteString.ByteString [Word.Word8] where
  from = ByteString.unpack

-- | Uses 'LazyByteString.fromStrict'.
instance From.From ByteString.ByteString LazyByteString.ByteString where
  from = LazyByteString.fromStrict

-- | Uses 'ShortByteString.toShort'.
instance From.From ByteString.ByteString ShortByteString.ShortByteString where
  from = ShortByteString.toShort

-- | Uses 'Text.decodeUtf8''.
instance TryCast.TryCast ByteString.ByteString Text.Text where
  tryCast = Utility.eitherTryCast Text.decodeUtf8'

-- LazyByteString

-- | Uses 'LazyByteString.pack'.
instance From.From [Word.Word8] LazyByteString.ByteString where
  from = LazyByteString.pack

-- | Uses 'LazyByteString.unpack'.
instance From.From LazyByteString.ByteString [Word.Word8] where
  from = LazyByteString.unpack

-- | Uses 'LazyByteString.toStrict'.
instance From.From LazyByteString.ByteString ByteString.ByteString where
  from = LazyByteString.toStrict

-- | Uses 'LazyText.decodeUtf8''.
instance TryCast.TryCast LazyByteString.ByteString LazyText.Text where
  tryCast = Utility.eitherTryCast LazyText.decodeUtf8'

-- ShortByteString

-- | Uses 'ShortByteString.pack'.
instance From.From [Word.Word8] ShortByteString.ShortByteString where
  from = ShortByteString.pack

-- | Uses 'ShortByteString.unpack'.
instance From.From ShortByteString.ShortByteString [Word.Word8] where
  from = ShortByteString.unpack

-- | Uses 'ShortByteString.fromShort'.
instance From.From ShortByteString.ShortByteString ByteString.ByteString where
  from = ShortByteString.fromShort

-- Text

-- | Uses 'Text.pack'. Some 'Char' values cannot be represented in 'Text.Text'
-- and will be replaced with @'\\xFFFD'@.
instance From.From String Text.Text where
  from = Text.pack

-- | Uses 'Text.unpack'.
instance From.From Text.Text String where
  from = Text.unpack

-- | Uses 'LazyText.fromStrict'.
instance From.From Text.Text LazyText.Text where
  from = LazyText.fromStrict

-- | Uses 'Text.encodeUtf8'.
instance From.From Text.Text ByteString.ByteString where
  from = Text.encodeUtf8

-- LazyText

-- | Uses 'LazyText.pack'. Some 'Char' values cannot be represented in
-- 'LazyText.Text' and will be replaced with @'\\xFFFD'@.
instance From.From String LazyText.Text where
  from = LazyText.pack

-- | Uses 'LazyText.unpack'.
instance From.From LazyText.Text String where
  from = LazyText.unpack

-- | Uses 'LazyText.toStrict'.
instance From.From LazyText.Text Text.Text where
  from = LazyText.toStrict

-- | Uses 'LazyText.encodeUtf8'.
instance From.From LazyText.Text LazyByteString.ByteString where
  from = LazyText.encodeUtf8

-- TryCastException

-- | Uses @coerce@.
instance From.From
  (TryCastException.TryCastException s u)
  (TryCastException.TryCastException s t)

-- | Uses 'show'.
instance
  ( Show s
  , Typeable.Typeable s
  , Typeable.Typeable t
  ) => From.From (TryCastException.TryCastException s t) String where
  from = show

-- | Converts via 'String'.
instance
  ( Show s
  , Typeable.Typeable s
  , Typeable.Typeable t
  ) => From.From (TryCastException.TryCastException s t) Text.Text where
  from = Utility.via @String

-- | Converts via 'String'.
instance
  ( Show s
  , Typeable.Typeable s
  , Typeable.Typeable t
  ) => From.From (TryCastException.TryCastException s t) LazyText.Text where
  from = Utility.via @String

fromNonNegativeIntegral
  :: (Integral s, Num t) => s -> Either Exception.ArithException t
fromNonNegativeIntegral x =
  if x < 0 then Left Exception.Underflow else Right $ fromIntegral x

-- | The maximum integral value that can be unambiguously represented as a
-- 'Float'. Equal to 16,777,215.
maxFloat :: Num a => a
maxFloat = 16777215

-- | The maximum integral value that can be unambiguously represented as a
-- 'Double'. Equal to 9,007,199,254,740,991.
maxDouble :: Num a => a
maxDouble = 9007199254740991
