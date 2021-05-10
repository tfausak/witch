{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.Complex as Complex
import qualified Data.Fixed as Fixed
import qualified Data.Int as Int
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import qualified Test.Hspec as Hspec
import qualified Witch

main :: IO ()
main = Hspec.hspec . Hspec.describe "Witch" $ do

  Hspec.describe "From" $ do

    Hspec.describe "from" $ do
      test $ Witch.from (1 :: Int.Int8) `Hspec.shouldBe` (1 :: Int.Int16)

  Hspec.describe "TryFrom" $ do

    Hspec.describe "tryFrom" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Int.Int8
      test $ f 1 `Hspec.shouldBe` Just 1
      test $ f 128 `Hspec.shouldBe` Nothing

  Hspec.describe "Utility" $ do

    Hspec.describe "as" $ do
      test $ Witch.as @Int.Int8 1 `Hspec.shouldBe` 1

    Hspec.describe "from" $ do
      test $ Witch.from @Int.Int8 1 `Hspec.shouldBe` (1 :: Int.Int16)

    Hspec.describe "into" $ do
      test $ Witch.into @Int.Int16 (1 :: Int.Int8) `Hspec.shouldBe` 1

    Hspec.describe "over" $ do
      test $ Witch.over @Int.Int8 (+ 1) (Age 1) `Hspec.shouldBe` Age 2

    Hspec.describe "via" $ do
      test $ Witch.via @Int.Int16 (1 :: Int.Int8) `Hspec.shouldBe` (1 :: Int.Int32)

    Hspec.describe "tryFrom" $ do
      test $ hush (Witch.tryFrom @Int.Int16 1) `Hspec.shouldBe` Just (1 :: Int.Int8)

    Hspec.describe "tryInto" $ do
      test $ hush (Witch.tryInto @Int.Int8 (1 :: Int.Int16)) `Hspec.shouldBe` Just 1

    Hspec.describe "unsafeCast" $ do
      test $ Witch.unsafeCast (1 :: Int.Int16) `Hspec.shouldBe` (1 :: Int.Int8)
      test $ Exception.evaluate (Witch.unsafeCast @Int.Int16 @Int.Int8 128) `Hspec.shouldThrow` Hspec.anyException

    Hspec.describe "unsafeFrom" $ do
      test $ Witch.unsafeFrom @Int.Int16 1 `Hspec.shouldBe` (1 :: Int.Int8)

    Hspec.describe "unsafeInto" $ do
      test $ Witch.unsafeInto @Int.Int8 (1 :: Int.Int16) `Hspec.shouldBe` 1

  Hspec.describe "Lift" $ do

    Hspec.describe "liftedCast" $ do
      test $ ($$(Witch.liftedCast (1 :: Int.Int16)) :: Int.Int8) `Hspec.shouldBe` 1

    Hspec.describe "liftedFrom" $ do
      test $ ($$(Witch.liftedFrom @Int.Int16 1) :: Int.Int8) `Hspec.shouldBe` 1

    Hspec.describe "liftedInto" $ do
      test $ $$(Witch.liftedInto @Int.Int8 (1 :: Int.Int16)) `Hspec.shouldBe` 1

  Hspec.describe "Instances" $ do

    -- Int8

    Hspec.describe "From Int8 Int16" $ do
      let f = Witch.from @Int.Int8 @Int.Int16
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 127 `Hspec.shouldBe` 127
      test $ f (-128) `Hspec.shouldBe` (-128)

    Hspec.describe "From Int8 Int32" $ do
      let f = Witch.from @Int.Int8 @Int.Int32
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 127 `Hspec.shouldBe` 127
      test $ f (-128) `Hspec.shouldBe` (-128)

    Hspec.describe "From Int8 Int64" $ do
      let f = Witch.from @Int.Int8 @Int.Int64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 127 `Hspec.shouldBe` 127
      test $ f (-128) `Hspec.shouldBe` (-128)

    Hspec.describe "From Int8 Int" $ do
      let f = Witch.from @Int.Int8 @Int
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 127 `Hspec.shouldBe` 127
      test $ f (-128) `Hspec.shouldBe` (-128)

    Hspec.describe "From Int8 Integer" $ do
      let f = Witch.from @Int.Int8 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 127 `Hspec.shouldBe` 127
      test $ f (-128) `Hspec.shouldBe` (-128)

    Hspec.describe "TryFrom Int8 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int8 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int8 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int8 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int8 Word" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int8 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "From Int8 Float" $ do
      let f = Witch.from @Int.Int8 @Float
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 127 `Hspec.shouldBe` 127
      test $ f (-128) `Hspec.shouldBe` (-128)

    Hspec.describe "From Int8 Double" $ do
      let f = Witch.from @Int.Int8 @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 127 `Hspec.shouldBe` 127
      test $ f (-128) `Hspec.shouldBe` (-128)

    -- Int16

    Hspec.describe "TryFrom Int16 Int8" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing
      test $ f (-128) `Hspec.shouldBe` Just (-128)
      test $ f (-129) `Hspec.shouldBe` Nothing

    Hspec.describe "From Int16 Int32" $ do
      let f = Witch.from @Int.Int16 @Int.Int32
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 32767 `Hspec.shouldBe` 32767
      test $ f (-32768) `Hspec.shouldBe` (-32768)

    Hspec.describe "From Int16 Int64" $ do
      let f = Witch.from @Int.Int16 @Int.Int64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 32767 `Hspec.shouldBe` 32767
      test $ f (-32768) `Hspec.shouldBe` (-32768)

    Hspec.describe "From Int16 Int" $ do
      let f = Witch.from @Int.Int16 @Int
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 32767 `Hspec.shouldBe` 32767
      test $ f (-32768) `Hspec.shouldBe` (-32768)

    Hspec.describe "From Int16 Integer" $ do
      let f = Witch.from @Int.Int16 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 32767 `Hspec.shouldBe` 32767
      test $ f (-32768) `Hspec.shouldBe` (-32768)

    Hspec.describe "TryFrom Int16 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int16 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int16 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int16 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int16 Word" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int16 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "From Int16 Float" $ do
      let f = Witch.from @Int.Int16 @Float
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 32767 `Hspec.shouldBe` 32767
      test $ f (-32768) `Hspec.shouldBe` (-32768)

    Hspec.describe "From Int16 Double" $ do
      let f = Witch.from @Int.Int16 @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 32767 `Hspec.shouldBe` 32767
      test $ f (-32768) `Hspec.shouldBe` (-32768)

    -- Int32

    Hspec.describe "TryFrom Int32 Int8" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing
      test $ f (-128) `Hspec.shouldBe` Just (-128)
      test $ f (-129) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int32 Int16" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing
      test $ f (-32768) `Hspec.shouldBe` Just (-32768)
      test $ f (-32769) `Hspec.shouldBe` Nothing

    Hspec.describe "From Int32 Int64" $ do
      let f = Witch.from @Int.Int32 @Int.Int64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 2147483647 `Hspec.shouldBe` 2147483647
      test $ f (-2147483648) `Hspec.shouldBe` (-2147483648)

    Hspec.describe "TryFrom Int32 Int" $ do
      Monad.when (toInteger (maxBound :: Int) < 2147483647) untested
      let f = hush . Witch.tryFrom @Int.Int32 @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f (-2147483648) `Hspec.shouldBe` Just (-2147483648)

    Hspec.describe "From Int32 Integer" $ do
      let f = Witch.from @Int.Int32 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 2147483647 `Hspec.shouldBe` 2147483647
      test $ f (-2147483648) `Hspec.shouldBe` (-2147483648)

    Hspec.describe "TryFrom Int32 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int32 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int32 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int32 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int32 Word" $ do
      Monad.when (toInteger (maxBound :: Word) < 2147483647) untested
      let f = hush . Witch.tryFrom @Int.Int32 @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int32 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int32 Float" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing

    Hspec.describe "From Int32 Double" $ do
      let f = Witch.from @Int.Int32 @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 2147483647 `Hspec.shouldBe` 2147483647
      test $ f (-2147483648) `Hspec.shouldBe` (-2147483648)

    -- Int64

    Hspec.describe "TryFrom Int64 Int8" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing
      test $ f (-128) `Hspec.shouldBe` Just (-128)
      test $ f (-129) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Int16" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing
      test $ f (-32768) `Hspec.shouldBe` Just (-32768)
      test $ f (-32769) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Int32" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing
      test $ f (-2147483648) `Hspec.shouldBe` Just (-2147483648)
      test $ f (-2147483649) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Int" $ do
      Monad.when (toInteger (maxBound :: Int) < 9223372036854775807) untested
      let f = hush . Witch.tryFrom @Int.Int64 @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f (-9223372036854775808) `Hspec.shouldBe` Just (-9223372036854775808)

    Hspec.describe "From Int64 Integer" $ do
      let f = Witch.from @Int.Int64 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 9223372036854775807 `Hspec.shouldBe` 9223372036854775807
      test $ f (-9223372036854775808) `Hspec.shouldBe` (-9223372036854775808)

    Hspec.describe "TryFrom Int64 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Word" $ do
      Monad.when (toInteger (maxBound :: Word) < 9223372036854775807) untested
      let f = hush . Witch.tryFrom @Int.Int64 @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Float" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int64 Double" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Double
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (-9007199254740991) `Hspec.shouldBe` Just (-9007199254740991)
      test $ f (-9007199254740992) `Hspec.shouldBe` Nothing

    -- Int

    Hspec.describe "TryFrom Int Int8" $ do
      let f = hush . Witch.tryFrom @Int @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing
      test $ f (-128) `Hspec.shouldBe` Just (-128)
      test $ f (-129) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Int16" $ do
      let f = hush . Witch.tryFrom @Int @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing
      test $ f (-32768) `Hspec.shouldBe` Just (-32768)
      test $ f (-32769) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Int32" $ do
      Monad.when (toInteger (maxBound :: Int) < 2147483647) untested
      let f = hush . Witch.tryFrom @Int @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing
      test $ f (-2147483648) `Hspec.shouldBe` Just (-2147483648)
      test $ f (-2147483649) `Hspec.shouldBe` Nothing

    Hspec.describe "From Int Int64" $ do
      let f = Witch.from @Int @Int.Int64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f maxBound `Hspec.shouldBe` fromIntegral (maxBound :: Int)
      test $ f minBound `Hspec.shouldBe` fromIntegral (minBound :: Int)

    Hspec.describe "From Int Integer" $ do
      let f = Witch.from @Int @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f maxBound `Hspec.shouldBe` fromIntegral (maxBound :: Int)
      test $ f minBound `Hspec.shouldBe` fromIntegral (minBound :: Int)

    Hspec.describe "TryFrom Int Word8" $ do
      let f = hush . Witch.tryFrom @Int @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Word16" $ do
      let f = hush . Witch.tryFrom @Int @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Word32" $ do
      Monad.when (toInteger (maxBound :: Int) < 4294967295) untested
      let f = hush . Witch.tryFrom @Int @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295
      test $ f 4294967296 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Word64" $ do
      let f = hush . Witch.tryFrom @Int @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f maxBound `Hspec.shouldBe` Just (fromIntegral (maxBound :: Int))
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Word" $ do
      let f = hush . Witch.tryFrom @Int @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f maxBound `Hspec.shouldBe` Just (fromIntegral (maxBound :: Int))
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Natural" $ do
      let f = hush . Witch.tryFrom @Int @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f maxBound `Hspec.shouldBe` Just (fromIntegral (maxBound :: Int))
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Float" $ do
      let f = hush . Witch.tryFrom @Int @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Int Double" $ do
      Monad.when (toInteger (maxBound :: Int) < 9007199254740991) untested
      let f = hush . Witch.tryFrom @Int @Double
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (-9007199254740991) `Hspec.shouldBe` Just (-9007199254740991)
      test $ f (-9007199254740992) `Hspec.shouldBe` Nothing

    -- Integer

    Hspec.describe "TryFrom Integer Int8" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing
      test $ f (-128) `Hspec.shouldBe` Just (-128)
      test $ f (-129) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Int16" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing
      test $ f (-32768) `Hspec.shouldBe` Just (-32768)
      test $ f (-32769) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Int32" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing
      test $ f (-2147483648) `Hspec.shouldBe` Just (-2147483648)
      test $ f (-2147483649) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Int64" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f 9223372036854775808 `Hspec.shouldBe` Nothing
      test $ f (-9223372036854775808) `Hspec.shouldBe` Just (-9223372036854775808)
      test $ f (-9223372036854775809) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Int" $ do
      let f = hush . Witch.tryFrom @Integer @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ let x = maxBound :: Int in f (fromIntegral x) `Hspec.shouldBe` Just x
      test $ let x = toInteger (maxBound :: Int) + 1 in f x `Hspec.shouldBe` Nothing
      test $ let x = minBound :: Int in f (fromIntegral x) `Hspec.shouldBe` Just x
      test $ let x = toInteger (minBound :: Int) - 1 in f x `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Word8" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Word16" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Word32" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295
      test $ f 4294967296 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Word64" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 18446744073709551615 `Hspec.shouldBe` Just 18446744073709551615
      test $ f 18446744073709551616 `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Word" $ do
      let f = hush . Witch.tryFrom @Integer @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ let x = maxBound :: Word in f (fromIntegral x) `Hspec.shouldBe` Just x
      test $ let x = toInteger (maxBound :: Word) + 1 in f x `Hspec.shouldBe` Nothing
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Natural" $ do
      let f = hush . Witch.tryFrom @Integer @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 18446744073709551616 `Hspec.shouldBe` Just 18446744073709551616
      test $ f (-1) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Float" $ do
      let f = hush . Witch.tryFrom @Integer @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Integer Double" $ do
      let f = hush . Witch.tryFrom @Integer @Double
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (-9007199254740991) `Hspec.shouldBe` Just (-9007199254740991)
      test $ f (-9007199254740992) `Hspec.shouldBe` Nothing

    -- Word8

    Hspec.describe "From Word8 Word16" $ do
      let f = Witch.from @Word.Word8 @Word.Word16
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Word32" $ do
      let f = Witch.from @Word.Word8 @Word.Word32
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Word64" $ do
      let f = Witch.from @Word.Word8 @Word.Word64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Word" $ do
      let f = Witch.from @Word.Word8 @Word
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Natural" $ do
      let f = Witch.from @Word.Word8 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "TryFrom Word8 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word8 @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing

    Hspec.describe "From Word8 Int16" $ do
      let f = Witch.from @Word.Word8 @Int.Int16
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Int32" $ do
      let f = Witch.from @Word.Word8 @Int.Int32
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Int64" $ do
      let f = Witch.from @Word.Word8 @Int.Int64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Int" $ do
      let f = Witch.from @Word.Word8 @Int
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Integer" $ do
      let f = Witch.from @Word.Word8 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Float" $ do
      let f = Witch.from @Word.Word8 @Float
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    Hspec.describe "From Word8 Double" $ do
      let f = Witch.from @Word.Word8 @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 255 `Hspec.shouldBe` 255

    -- Word16

    Hspec.describe "TryFrom Word16 Word8" $ do
      let f = hush . Witch.tryFrom @Word.Word16 @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing

    Hspec.describe "From Word16 Word32" $ do
      let f = Witch.from @Word.Word16 @Word.Word32
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Word64" $ do
      let f = Witch.from @Word.Word16 @Word.Word64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Word" $ do
      let f = Witch.from @Word.Word16 @Word
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Natural" $ do
      let f = Witch.from @Word.Word16 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "TryFrom Word16 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word16 @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word16 Int16" $ do
      let f = hush . Witch.tryFrom @Word.Word16 @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing

    Hspec.describe "From Word16 Int32" $ do
      let f = Witch.from @Word.Word16 @Int.Int32
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Int64" $ do
      let f = Witch.from @Word.Word16 @Int.Int64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Int" $ do
      let f = Witch.from @Word.Word16 @Int
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Integer" $ do
      let f = Witch.from @Word.Word16 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Float" $ do
      let f = Witch.from @Word.Word16 @Float
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    Hspec.describe "From Word16 Double" $ do
      let f = Witch.from @Word.Word16 @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 65535 `Hspec.shouldBe` 65535

    -- Word32

    Hspec.describe "TryFrom Word32 Word8" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word32 Word16" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing

    Hspec.describe "From Word32 Word64" $ do
      let f = Witch.from @Word.Word32 @Word.Word64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 4294967295 `Hspec.shouldBe` 4294967295

    Hspec.describe "TryFrom Word32 Word" $ do
      Monad.when (toInteger (maxBound :: Word) < 4294967295) untested
      let f = hush . Witch.tryFrom @Word.Word32 @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295

    Hspec.describe "From Word32 Natural" $ do
      let f = Witch.from @Word.Word32 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 4294967295 `Hspec.shouldBe` 4294967295

    Hspec.describe "TryFrom Word32 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word32 Int16" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word32 Int32" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing

    Hspec.describe "From Word32 Int64" $ do
      let f = Witch.from @Word.Word32 @Int.Int64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 4294967295 `Hspec.shouldBe` 4294967295

    Hspec.describe "TryFrom Word32 Int" $ do
      Monad.when (toInteger (maxBound :: Int) < 4294967295) untested
      let f = hush . Witch.tryFrom @Word.Word32 @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295

    Hspec.describe "From Word32 Integer" $ do
      let f = Witch.from @Word.Word32 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 4294967295 `Hspec.shouldBe` 4294967295

    Hspec.describe "TryFrom Word32 Float" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing

    Hspec.describe "From Word32 Double" $ do
      let f = Witch.from @Word.Word32 @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 4294967295 `Hspec.shouldBe` 4294967295

    -- Word64

    Hspec.describe "TryFrom Word64 Word8" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Word16" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Word32" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295
      test $ f 4294967296 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Word" $ do
      Monad.when (toInteger (maxBound :: Word) < 18446744073709551615) untested
      let f = hush . Witch.tryFrom @Word.Word64 @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 18446744073709551615 `Hspec.shouldBe` Just 18446744073709551615

    Hspec.describe "From Word64 Natural" $ do
      let f = Witch.from @Word.Word64 @Natural.Natural
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 18446744073709551615 `Hspec.shouldBe` 18446744073709551615

    Hspec.describe "TryFrom Word64 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Int16" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Int32" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Int64" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f 9223372036854775808 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Int" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ let x = maxBound :: Int in hush (Witch.tryFrom @Word.Word64 @Int (fromIntegral x)) `Hspec.shouldBe` Just x
      test $ let x = fromIntegral (maxBound :: Int) + 1 :: Word.Word64 in hush (Witch.tryFrom @Word.Word64 @Int x) `Hspec.shouldBe` Nothing

    Hspec.describe "From Word64 Integer" $ do
      let f = Witch.from @Word.Word64 @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 18446744073709551615 `Hspec.shouldBe` 18446744073709551615

    Hspec.describe "TryFrom Word64 Float" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word64 Double" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Double
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing

    -- Word

    Hspec.describe "TryFrom Word Word8" $ do
      let f = hush . Witch.tryFrom @Word @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word Word16" $ do
      let f = hush . Witch.tryFrom @Word @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word Word32" $ do
      Monad.when (toInteger (maxBound :: Word) < 4294967295) untested
      let f = hush . Witch.tryFrom @Word @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295
      test $ f 4294967296 `Hspec.shouldBe` Nothing

    Hspec.describe "From Word Word64" $ do
      let f = Witch.from @Word @Word.Word64
      test $ f 0 `Hspec.shouldBe` 0
      test $ f maxBound `Hspec.shouldBe` fromIntegral (maxBound :: Word)

    Hspec.describe "From Word Natural" $ do
      let f = Witch.from @Word @Natural.Natural
      test $ f 0 `Hspec.shouldBe` 0
      test $ f maxBound `Hspec.shouldBe` fromIntegral (maxBound :: Word)

    Hspec.describe "TryFrom Word Int8" $ do
      let f = hush . Witch.tryFrom @Word @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word Int16" $ do
      let f = hush . Witch.tryFrom @Word @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word Int32" $ do
      Monad.when (toInteger (maxBound :: Word) < 2147483647) untested
      let f = hush . Witch.tryFrom @Word @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word Int64" $ do
      Monad.when (toInteger (maxBound :: Word) < 9223372036854775807) untested
      let f = hush . Witch.tryFrom @Word @Int.Int64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f 9223372036854775808 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word Int" $ do
      let f = hush . Witch.tryFrom @Word @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ let x = maxBound :: Int in hush (Witch.tryFrom @Word @Int (fromIntegral x)) `Hspec.shouldBe` Just x
      test $ let x = fromIntegral (maxBound :: Int) + 1 :: Word in hush (Witch.tryFrom @Word @Int x) `Hspec.shouldBe` Nothing

    Hspec.describe "From Word Integer" $ do
      let f = Witch.from @Word @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f maxBound `Hspec.shouldBe` fromIntegral (maxBound :: Word)

    Hspec.describe "TryFrom Word Float" $ do
      let f = hush . Witch.tryFrom @Word @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Word Double" $ do
      Monad.when (toInteger (maxBound :: Word) < 9007199254740991) untested
      let f = hush . Witch.tryFrom @Word @Double
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing

    -- Natural

    Hspec.describe "TryFrom Natural Word8" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Word16" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Word32" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295
      test $ f 4294967296 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Word64" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 18446744073709551615 `Hspec.shouldBe` Just 18446744073709551615
      test $ f 18446744073709551616 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Word" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ let x = maxBound :: Word in hush (Witch.tryFrom @Natural.Natural @Word (fromIntegral x)) `Hspec.shouldBe` Just x
      test $ let x = fromIntegral (maxBound :: Word) + 1 :: Natural.Natural in hush (Witch.tryFrom @Natural.Natural @Word x) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Int8" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Int16" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Int32" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Int64" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9223372036854775807 `Hspec.shouldBe` Just 9223372036854775807
      test $ f 9223372036854775808 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Int" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ let x = maxBound :: Int in hush (Witch.tryFrom @Natural.Natural @Int (fromIntegral x)) `Hspec.shouldBe` Just x
      test $ let x = fromIntegral (maxBound :: Int) + 1 :: Natural.Natural in hush (Witch.tryFrom @Natural.Natural @Int x) `Hspec.shouldBe` Nothing

    Hspec.describe "From Natural Integer" $ do
      let f = Witch.from @Natural.Natural @Integer
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 9223372036854775808 `Hspec.shouldBe` 9223372036854775808

    Hspec.describe "TryFrom Natural Float" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Float
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Natural Double" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Double
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing

    -- Float

    Hspec.describe "TryFrom Float Int8" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing
      test $ f (-128) `Hspec.shouldBe` Just (-128)
      test $ f (-129) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Int16" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing
      test $ f (-32768) `Hspec.shouldBe` Just (-32768)
      test $ f (-32769) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Int32" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Int64" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Int" $ do
      let f = hush . Witch.tryFrom @Float @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Integer" $ do
      let f = hush . Witch.tryFrom @Float @Integer
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f (-16777216) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Word8" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Word16" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Word32" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Word64" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Word" $ do
      let f = hush . Witch.tryFrom @Float @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Natural" $ do
      let f = hush . Witch.tryFrom @Float @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f 16777216 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Float Rational" $ do
      let f = hush . Witch.tryFrom @Float @Rational
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f (-0) `Hspec.shouldBe` Just 0
      test $ f 0.5 `Hspec.shouldBe` Just 0.5
      test $ f (-0.5) `Hspec.shouldBe` Just (-0.5)
      test $ f 16777215 `Hspec.shouldBe` Just 16777215
      test $ f (-16777215) `Hspec.shouldBe` Just (-16777215)
      test $ f 16777216 `Hspec.shouldBe` Just 16777216
      test $ f (-16777216) `Hspec.shouldBe` Just (-16777216)
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "From Float Double" $ do
      let f = Witch.from @Float @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 0.5 `Hspec.shouldBe` 0.5
      test $ f (-0.5) `Hspec.shouldBe` (-0.5)
      test $ f (0 / 0) `Hspec.shouldSatisfy` isNaN
      test $ f (1 / 0) `Hspec.shouldBe` (1 / 0)
      test $ f (-1 / 0) `Hspec.shouldBe` (-1 / 0)

    -- Double

    Hspec.describe "TryFrom Double Int8" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 127 `Hspec.shouldBe` Just 127
      test $ f 128 `Hspec.shouldBe` Nothing
      test $ f (-128) `Hspec.shouldBe` Just (-128)
      test $ f (-129) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Int16" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 32767 `Hspec.shouldBe` Just 32767
      test $ f 32768 `Hspec.shouldBe` Nothing
      test $ f (-32768) `Hspec.shouldBe` Just (-32768)
      test $ f (-32769) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Int32" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 2147483647 `Hspec.shouldBe` Just 2147483647
      test $ f 2147483648 `Hspec.shouldBe` Nothing
      test $ f (-2147483648) `Hspec.shouldBe` Just (-2147483648)
      test $ f (-2147483649) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Int64" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (-9007199254740991) `Hspec.shouldBe` Just (-9007199254740991)
      test $ f (-9007199254740992) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Int" $ do
      Monad.when (toInteger (maxBound :: Int) < 9007199254740991) untested
      let f = hush . Witch.tryFrom @Double @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (-9007199254740991) `Hspec.shouldBe` Just (-9007199254740991)
      test $ f (-9007199254740992) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Integer" $ do
      let f = hush . Witch.tryFrom @Double @Integer
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (-9007199254740991) `Hspec.shouldBe` Just (-9007199254740991)
      test $ f (-9007199254740992) `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Word8" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word8
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 255 `Hspec.shouldBe` Just 255
      test $ f 256 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Word16" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word16
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 65535 `Hspec.shouldBe` Just 65535
      test $ f 65536 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Word32" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word32
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 4294967295 `Hspec.shouldBe` Just 4294967295
      test $ f 4294967296 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Word64" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word64
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Word" $ do
      Monad.when (toInteger (maxBound :: Word) < 9007199254740991) untested
      let f = hush . Witch.tryFrom @Double @Word
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Natural" $ do
      let f = hush . Witch.tryFrom @Double @Natural.Natural
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f 9007199254740992 `Hspec.shouldBe` Nothing
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "TryFrom Double Rational" $ do
      let f = hush . Witch.tryFrom @Double @Rational
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f (-0) `Hspec.shouldBe` Just 0
      test $ f 0.5 `Hspec.shouldBe` Just 0.5
      test $ f (-0.5) `Hspec.shouldBe` Just (-0.5)
      test $ f 9007199254740991 `Hspec.shouldBe` Just 9007199254740991
      test $ f (-9007199254740991) `Hspec.shouldBe` Just (-9007199254740991)
      test $ f 9007199254740992 `Hspec.shouldBe` Just 9007199254740992
      test $ f (-9007199254740992) `Hspec.shouldBe` Just (-9007199254740992)
      test $ f (0 / 0) `Hspec.shouldBe` Nothing
      test $ f (1 / 0) `Hspec.shouldBe` Nothing
      test $ f (-1 / 0) `Hspec.shouldBe` Nothing

    Hspec.describe "From Double Float" $ do
      let f = Witch.from @Double @Float
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 0.5 `Hspec.shouldBe` 0.5
      test $ f (-0.5) `Hspec.shouldBe` (-0.5)
      test $ f (0 / 0) `Hspec.shouldSatisfy` isNaN
      test $ f (1 / 0) `Hspec.shouldBe` (1 / 0)
      test $ f (-1 / 0) `Hspec.shouldBe` (-1 / 0)

    -- Ratio

    Hspec.describe "From a (Ratio a)" $ do
      test $ Witch.from @Integer @Rational 0 `Hspec.shouldBe` 0
      let f = Witch.from @Int @(Ratio.Ratio Int)
      test $ f 0 `Hspec.shouldBe` 0

    Hspec.describe "TryFrom (Ratio a) a" $ do
      test $ hush (Witch.tryFrom @Rational @Integer 0) `Hspec.shouldBe` Just 0
      test $ hush (Witch.tryFrom @Rational @Integer 0.5) `Hspec.shouldBe` Nothing
      let f = hush . Witch.tryFrom @(Ratio.Ratio Int) @Int
      test $ f 0 `Hspec.shouldBe` Just 0
      test $ f 0.5 `Hspec.shouldBe` Nothing

    Hspec.describe "From Rational Float" $ do
      let f = Witch.from @Rational @Float
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 0.5 `Hspec.shouldBe` 0.5
      test $ f (-0.5) `Hspec.shouldBe` (-0.5)

    Hspec.describe "From Rational Double" $ do
      let f = Witch.from @Rational @Double
      test $ f 0 `Hspec.shouldBe` 0
      test $ f 0.5 `Hspec.shouldBe` 0.5
      test $ f (-0.5) `Hspec.shouldBe` (-0.5)

    -- Fixed

    Hspec.describe "From Integer (Fixed a)" $ do
      test $ Witch.from @Integer @Fixed.Uni 1 `Hspec.shouldBe` 1
      let f = Witch.from @Integer @Fixed.Deci
      test $ f 1 `Hspec.shouldBe` 0.1

    Hspec.describe "From (Fixed a) Integer" $ do
      test $ Witch.from @Fixed.Uni @Integer 1 `Hspec.shouldBe` 1
      let f = Witch.from @Fixed.Deci @Integer
      test $ f 1 `Hspec.shouldBe` 10

    -- Complex

    Hspec.describe "From a (Complex a)" $ do
      test $ Witch.from @Double @(Complex.Complex Double) 1 `Hspec.shouldBe` 1
      let f = Witch.from @Float @(Complex.Complex Float)
      test $ f 1 `Hspec.shouldBe` 1

    Hspec.describe "TryFrom (Complex a) a" $ do
      test $ hush (Witch.tryFrom @(Complex.Complex Double) @Double 1) `Hspec.shouldBe` Just 1
      test $ hush (Witch.tryFrom @(Complex.Complex Double) @Double (0 Complex.:+ 1)) `Hspec.shouldBe` Nothing
      let f = hush . Witch.tryFrom @(Complex.Complex Float) @Float
      test $ f 1 `Hspec.shouldBe` Just 1
      test $ f (0 Complex.:+ 1) `Hspec.shouldBe` Nothing

    -- NonEmpty

    Hspec.describe "TryFrom [a] (NonEmpty a)" $ do
      let f = hush . Witch.tryFrom @[Int] @(NonEmpty.NonEmpty Int)
      test $ f [] `Hspec.shouldBe` Nothing
      test $ f [1] `Hspec.shouldBe` Just (1 NonEmpty.:| [])
      test $ f [1, 2] `Hspec.shouldBe` Just (1 NonEmpty.:| [2])

    Hspec.describe "From (NonEmpty a) [a]" $ do
      let f = Witch.from @(NonEmpty.NonEmpty Int) @[Int]
      test $ f (1 NonEmpty.:| []) `Hspec.shouldBe` [1]
      test $ f (1 NonEmpty.:| [2]) `Hspec.shouldBe` [1, 2]

    -- Set

    Hspec.describe "From [a] (Set a)" $ do
      let f = Witch.from @[Char] @(Set.Set Char)
      test $ f [] `Hspec.shouldBe` Set.fromList []
      test $ f ['a'] `Hspec.shouldBe` Set.fromList ['a']
      test $ f ['a', 'b'] `Hspec.shouldBe` Set.fromList ['a', 'b']
      test $ f ['a', 'a'] `Hspec.shouldBe` Set.fromList ['a']

    Hspec.describe "From (Set a) [a]" $ do
      let f = Witch.from @(Set.Set Char) @[Char]
      test $ f (Set.fromList []) `Hspec.shouldBe` []
      test $ f (Set.fromList ['a']) `Hspec.shouldBe` ['a']
      test $ f (Set.fromList ['a', 'b']) `Hspec.shouldBe` ['a', 'b']

    -- IntSet

    Hspec.describe "From [Int] IntSet" $ do
      let f = Witch.from @[Int] @IntSet.IntSet
      test $ f [] `Hspec.shouldBe` IntSet.fromList []
      test $ f [1] `Hspec.shouldBe` IntSet.fromList [1]
      test $ f [1, 2] `Hspec.shouldBe` IntSet.fromList [1, 2]

    Hspec.describe "From IntSet [Int]" $ do
      let f = Witch.from @IntSet.IntSet @[Int]
      test $ f (IntSet.fromList []) `Hspec.shouldBe` []
      test $ f (IntSet.fromList [1]) `Hspec.shouldBe` [1]
      test $ f (IntSet.fromList [1, 2]) `Hspec.shouldBe` [1, 2]

    -- Map

    Hspec.describe "From [(k, v)] (Map k v)" $ do
      let f = Witch.from @[(Char, Int)] @(Map.Map Char Int)
      test $ f [] `Hspec.shouldBe` Map.empty
      test $ f [('a', 1)] `Hspec.shouldBe` Map.fromList [('a', 1)]
      test $ f [('a', 1), ('b', 2)] `Hspec.shouldBe` Map.fromList [('a', 1), ('b', 2)]
      test $ f [('a', 1), ('a', 2)] `Hspec.shouldBe` Map.fromList [('a', 2)]

    Hspec.describe "From (Map k v) [(k, v)]" $ do
      let f = Witch.from @(Map.Map Char Int) @[(Char, Int)]
      test $ f Map.empty `Hspec.shouldBe` []
      test $ f (Map.fromList [('a', 1)]) `Hspec.shouldBe` [('a', 1)]
      test $ f (Map.fromList [('a', 1), ('b', 2)]) `Hspec.shouldBe` [('a', 1), ('b', 2)]

    -- IntMap

    Hspec.describe "From [(Int, v)] (IntMap v)" $ do
      let f = Witch.from @[(Int, Char)] @(IntMap.IntMap Char)
      test $ f [] `Hspec.shouldBe` IntMap.fromList []
      test $ f [(1, 'a')] `Hspec.shouldBe` IntMap.fromList [(1, 'a')]
      test $ f [(1, 'a'), (2, 'b')] `Hspec.shouldBe` IntMap.fromList [(1, 'a'), (2, 'b')]
      test $ f [(1, 'a'), (1, 'b')] `Hspec.shouldBe` IntMap.fromList [(1, 'b')]

    Hspec.describe "From (IntMap v) [(Int, v)]" $ do
      let f = Witch.from @(IntMap.IntMap Char) @[(Int, Char)]
      test $ f (IntMap.fromList []) `Hspec.shouldBe` []
      test $ f (IntMap.fromList [(1, 'a')]) `Hspec.shouldBe` [(1, 'a')]
      test $ f (IntMap.fromList [(1, 'a'), (2, 'b')]) `Hspec.shouldBe` [(1, 'a'), (2, 'b')]

    -- Seq

    Hspec.describe "From [a] (Seq a)" $ do
      let f = Witch.from @[Int] @(Seq.Seq Int)
      test $ f [] `Hspec.shouldBe` Seq.fromList []
      test $ f [1] `Hspec.shouldBe` Seq.fromList [1]
      test $ f [1, 2] `Hspec.shouldBe` Seq.fromList [1, 2]

    Hspec.describe "From (Seq a) [a]" $ do
      let f = Witch.from @(Seq.Seq Int) @[Int]
      test $ f (Seq.fromList []) `Hspec.shouldBe` []
      test $ f (Seq.fromList [1]) `Hspec.shouldBe` [1]
      test $ f (Seq.fromList [1, 2]) `Hspec.shouldBe` [1, 2]

    -- ByteString

    Hspec.describe "From [Word8] ByteString" $ do
      let f = Witch.from @[Word.Word8] @ByteString.ByteString
      test $ f [] `Hspec.shouldBe` ByteString.pack []
      test $ f [0x00] `Hspec.shouldBe` ByteString.pack [0x00]
      test $ f [0x0f, 0xf0] `Hspec.shouldBe` ByteString.pack [0x0f, 0xf0]

    Hspec.describe "From ByteString [Word8]" $ do
      let f = Witch.from @ByteString.ByteString @[Word.Word8]
      test $ f (ByteString.pack []) `Hspec.shouldBe` []
      test $ f (ByteString.pack [0x00]) `Hspec.shouldBe` [0x00]
      test $ f (ByteString.pack [0x0f, 0xf0]) `Hspec.shouldBe` [0x0f, 0xf0]

    Hspec.describe "From ByteString LazyByteString" $ do
      let f = Witch.from @ByteString.ByteString @LazyByteString.ByteString
      test $ f (ByteString.pack []) `Hspec.shouldBe` LazyByteString.pack []
      test $ f (ByteString.pack [0x00]) `Hspec.shouldBe` LazyByteString.pack [0x00]
      test $ f (ByteString.pack [0x0f, 0xf0]) `Hspec.shouldBe` LazyByteString.pack [0x0f, 0xf0]

    Hspec.describe "From ByteString ShortByteString" $ do
      let f = Witch.from @ByteString.ByteString @ShortByteString.ShortByteString
      test $ f (ByteString.pack []) `Hspec.shouldBe` ShortByteString.pack []
      test $ f (ByteString.pack [0x00]) `Hspec.shouldBe` ShortByteString.pack [0x00]
      test $ f (ByteString.pack [0x0f, 0xf0]) `Hspec.shouldBe` ShortByteString.pack [0x0f, 0xf0]

    Hspec.describe "TryFrom ByteString Text" $ do
      let f = hush . Witch.tryFrom @ByteString.ByteString @Text.Text
      test $ f (ByteString.pack []) `Hspec.shouldBe` Just (Text.pack "")
      test $ f (ByteString.pack [0x61]) `Hspec.shouldBe` Just (Text.pack "a")
      test $ f (ByteString.pack [0xff]) `Hspec.shouldBe` Nothing

    -- LazyByteString

    Hspec.describe "From [Word8] LazyByteString" $ do
      let f = Witch.from @[Word.Word8] @LazyByteString.ByteString
      test $ f [] `Hspec.shouldBe` LazyByteString.pack []
      test $ f [0x00] `Hspec.shouldBe` LazyByteString.pack [0x00]
      test $ f [0x0f, 0xf0] `Hspec.shouldBe` LazyByteString.pack [0x0f, 0xf0]

    Hspec.describe "From LazyByteString [Word8]" $ do
      let f = Witch.from @LazyByteString.ByteString @[Word.Word8]
      test $ f (LazyByteString.pack []) `Hspec.shouldBe` []
      test $ f (LazyByteString.pack [0x00]) `Hspec.shouldBe` [0x00]
      test $ f (LazyByteString.pack [0x0f, 0xf0]) `Hspec.shouldBe` [0x0f, 0xf0]

    Hspec.describe "From LazyByteString ByteString" $ do
      let f = Witch.from @LazyByteString.ByteString @ByteString.ByteString
      test $ f (LazyByteString.pack []) `Hspec.shouldBe` ByteString.pack []
      test $ f (LazyByteString.pack [0x00]) `Hspec.shouldBe` ByteString.pack [0x00]
      test $ f (LazyByteString.pack [0x0f, 0xf0]) `Hspec.shouldBe` ByteString.pack [0x0f, 0xf0]

    Hspec.describe "TryFrom LazyByteString LazyText" $ do
      let f = hush . Witch.tryFrom @LazyByteString.ByteString @LazyText.Text
      test $ f (LazyByteString.pack []) `Hspec.shouldBe` Just (LazyText.pack "")
      test $ f (LazyByteString.pack [0x61]) `Hspec.shouldBe` Just (LazyText.pack "a")
      test $ f (LazyByteString.pack [0xff]) `Hspec.shouldBe` Nothing

    -- ShortByteString

    Hspec.describe "From [Word8] ShortByteString" $ do
      let f = Witch.from @[Word.Word8] @ShortByteString.ShortByteString
      test $ f [] `Hspec.shouldBe` ShortByteString.pack []
      test $ f [0x00] `Hspec.shouldBe` ShortByteString.pack [0x00]
      test $ f [0x0f, 0xf0] `Hspec.shouldBe` ShortByteString.pack [0x0f, 0xf0]

    Hspec.describe "From ShortByteString [Word8]" $ do
      let f = Witch.from @ShortByteString.ShortByteString @[Word.Word8]
      test $ f (ShortByteString.pack []) `Hspec.shouldBe` []
      test $ f (ShortByteString.pack [0x00]) `Hspec.shouldBe` [0x00]
      test $ f (ShortByteString.pack [0x0f, 0xf0]) `Hspec.shouldBe` [0x0f, 0xf0]

    Hspec.describe "From ShortByteString ByteString" $ do
      let f = Witch.from @ShortByteString.ShortByteString @ByteString.ByteString
      test $ f (ShortByteString.pack []) `Hspec.shouldBe` ByteString.pack []
      test $ f (ShortByteString.pack [0x00]) `Hspec.shouldBe` ByteString.pack [0x00]
      test $ f (ShortByteString.pack [0x0f, 0xf0]) `Hspec.shouldBe` ByteString.pack [0x0f, 0xf0]

    -- Text

    Hspec.describe "From String Text" $ do
      let f = Witch.from @String @Text.Text
      test $ f "" `Hspec.shouldBe` Text.pack ""
      test $ f "a" `Hspec.shouldBe` Text.pack "a"
      test $ f "ab" `Hspec.shouldBe` Text.pack "ab"

    Hspec.describe "From Text String" $ do
      let f = Witch.from @Text.Text @String
      test $ f (Text.pack "") `Hspec.shouldBe` ""
      test $ f (Text.pack "a") `Hspec.shouldBe` "a"
      test $ f (Text.pack "ab") `Hspec.shouldBe` "ab"

    Hspec.describe "From Text LazyText" $ do
      let f = Witch.from @Text.Text @LazyText.Text
      test $ f (Text.pack "") `Hspec.shouldBe` LazyText.pack ""
      test $ f (Text.pack "a") `Hspec.shouldBe` LazyText.pack "a"
      test $ f (Text.pack "ab") `Hspec.shouldBe` LazyText.pack "ab"

    Hspec.describe "From Text ByteString" $ do
      let f = Witch.from @Text.Text @ByteString.ByteString
      test $ f (Text.pack "") `Hspec.shouldBe` ByteString.pack []
      test $ f (Text.pack "a") `Hspec.shouldBe` ByteString.pack [0x61]

    -- LazyText

    Hspec.describe "From String LazyText" $ do
      let f = Witch.from @String @LazyText.Text
      test $ f "" `Hspec.shouldBe` LazyText.pack ""
      test $ f "a" `Hspec.shouldBe` LazyText.pack "a"
      test $ f "ab" `Hspec.shouldBe` LazyText.pack "ab"

    Hspec.describe "From LazyText String" $ do
      let f = Witch.from @LazyText.Text @String
      test $ f (LazyText.pack "") `Hspec.shouldBe` ""
      test $ f (LazyText.pack "a") `Hspec.shouldBe` "a"
      test $ f (LazyText.pack "ab") `Hspec.shouldBe` "ab"

    Hspec.describe "From LazyText Text" $ do
      let f = Witch.from @LazyText.Text @Text.Text
      test $ f (LazyText.pack "") `Hspec.shouldBe` Text.pack ""
      test $ f (LazyText.pack "a") `Hspec.shouldBe` Text.pack "a"
      test $ f (LazyText.pack "ab") `Hspec.shouldBe` Text.pack "ab"

    Hspec.describe "From LazyText LazyByteString" $ do
      let f = Witch.from @LazyText.Text @LazyByteString.ByteString
      test $ f (LazyText.pack "") `Hspec.shouldBe` LazyByteString.pack []
      test $ f (LazyText.pack "a") `Hspec.shouldBe` LazyByteString.pack [0x61]

    -- TryCastException

    Hspec.describe "From (TryCastException s t0) (TryCastException s t1)" $ do
      Hspec.it "needs tests" Hspec.pending

    Hspec.describe "From (TryCastException s t) String" $ do
      test $ Witch.from (Witch.TryCastException Nothing Nothing :: Witch.TryCastException (Maybe Bool) (Maybe Int)) `Hspec.shouldBe` "TryCastException @(Maybe Bool) @(Maybe Int) Nothing Nothing"
      let f = Witch.from @(Witch.TryCastException Bool Int) @String
      test $ f (Witch.TryCastException False Nothing) `Hspec.shouldBe` "TryCastException @Bool @Int False Nothing"

    Hspec.describe "From (TryCastException s t) Text" $ do
      let f = Witch.from @(Witch.TryCastException Bool Int) @Text.Text
      test $ f (Witch.TryCastException False Nothing) `Hspec.shouldBe` Text.pack "TryCastException @Bool @Int False Nothing"

    Hspec.describe "From (TryCastException s t) LazyText" $ do
      let f = Witch.from @(Witch.TryCastException Bool Int) @LazyText.Text
      test $ f (Witch.TryCastException False Nothing) `Hspec.shouldBe` LazyText.pack "TryCastException @Bool @Int False Nothing"

test :: Hspec.Example a => a -> Hspec.SpecWith (Hspec.Arg a)
test = Hspec.it ""

untested :: Hspec.SpecWith a
untested = Hspec.runIO $ Exception.throwIO Untested

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

data Untested
  = Untested
  deriving (Eq, Show)

instance Exception.Exception Untested

newtype Age
  = Age Int.Int8
  deriving (Eq, Show)

instance Witch.From Age Int.Int8

instance Witch.From Int.Int8 Age
