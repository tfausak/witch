{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.Instances where

import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import qualified Witch.Cast as Cast
import qualified Witch.TryCast as TryCast
import qualified Witch.TryCastException as TryCastException

-- []

instance TryCast.TryCast [a] (NonEmpty.NonEmpty a) where
  tryCast = maybeTryCast NonEmpty.nonEmpty

-- NonEmpty

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

maybeTryCast :: (s -> Maybe t) -> s -> Either (TryCastException.TryCastException s t) t
maybeTryCast f s = case f s of
  Nothing -> Left $ TryCastException.TryCastException s
  Just t -> Right t
