{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-error=overflowed-literals #-}

import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Writer as Writer
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
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Time.Clock.TAI as Time
import qualified Data.Tuple as Tuple
import qualified Data.Void as Void
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified GHC.Stack as Stack
import qualified Numeric.Natural as Natural
import qualified Test.HUnit as HUnit
import qualified Witch
import qualified Witch.Encoding as Encoding
import qualified Witch.Utility as Utility

main :: IO ()
main = HUnit.runTestTTAndExit $ specToTest spec

spec :: Spec
spec = describe "Witch" $ do
  describe "From" $ do
    describe "from" $ do
      it "works" $ do
        Witch.from @Int.Int8 @Int.Int16 1 `shouldBe` 1

  describe "TryFrom" $ do
    describe "tryFrom" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Int.Int8
      it "works" $ do
        f 1 `shouldBe` Just 1
        f 128 `shouldBe` Nothing

  describe "Utility" $ do
    describe "as" $ do
      it "works" $ do
        Utility.as @Int.Int8 1 `shouldBe` 1

    describe "into" $ do
      it "works" $ do
        Witch.into @Int.Int16 @Int.Int8 1 `shouldBe` 1

    describe "over" $ do
      it "works" $ do
        Utility.over @Int.Int8 (+ 1) (MkAge 1) `shouldBe` MkAge 2

    describe "via" $ do
      it "works" $ do
        Witch.via @Int.Int16 @Int.Int8 @Int.Int32 1 `shouldBe` 1

    describe "tryInto" $ do
      let f = hush . Witch.tryInto @Int.Int8 @Int.Int16
      it "works" $ do
        f 1 `shouldBe` Just 1
        f 128 `shouldBe` Nothing

    describe "tryVia" $ do
      let f = hush . Witch.tryVia @Int.Int16 @Int.Int32 @Int.Int8
      it "works" $ do
        f 1 `shouldBe` Just 1
        f 128 `shouldBe` Nothing
        f 32768 `shouldBe` Nothing

    describe "unsafeFrom" $ do
      let f = Witch.unsafeFrom @Int.Int16 @Int.Int8
      it "works" $ do
        f 1 `shouldBe` 1
        Exception.evaluate (f 128) `shouldThrow` anyTryFromException @Int.Int16 @Int.Int8

    describe "unsafeInto" $ do
      let f = Witch.unsafeInto @Int.Int8 @Int.Int16
      it "works" $ do
        f 1 `shouldBe` 1
        Exception.evaluate (f 128) `shouldThrow` anyTryFromException @Int.Int16 @Int.Int8

  describe "Lift" $ do
    describe "liftedFrom" $ do
      it "works" $ do
        $$(Witch.liftedFrom @Int.Int16 @Int.Int8 1) `shouldBe` 1

    describe "liftedInto" $ do
      it "works" $ do
        $$(Witch.liftedInto @Int.Int8 @Int.Int16 1) `shouldBe` 1

  describe "TryFromException" $ do
    describe "show" $ do
      it "works" $ do
        show (Witch.TryFromException @Int @Int 0 Nothing) `shouldBe` "TryFromException @Int @Int 0 Nothing"
        show
          ( Witch.TryFromException @(Seq.Seq Int) @(Seq.Seq Int)
              (Seq.fromList [])
              (Just (Exception.toException Exception.Overflow))
          )
          `shouldBe` "TryFromException @(Seq Int) @(Seq Int) (fromList []) (Just arithmetic overflow)"

  describe "Instances" $ do
    describe "From a a" $ do
      it "works" $ do
        Witch.from @Int @Int 0 `shouldBe` 0

    describe "From Int8 Int16" $ do
      let f = Witch.from @Int.Int8 @Int.Int16
      it "works" $ do
        f 0 `shouldBe` 0
        f 127 `shouldBe` 127
        f -128 `shouldBe` -128

    describe "From Int8 Int32" $ do
      let f = Witch.from @Int.Int8 @Int.Int32
      it "works" $ do
        f 0 `shouldBe` 0
        f 127 `shouldBe` 127
        f -128 `shouldBe` -128

    describe "From Int8 Int64" $ do
      let f = Witch.from @Int.Int8 @Int.Int64
      it "works" $ do
        f 0 `shouldBe` 0
        f 127 `shouldBe` 127
        f -128 `shouldBe` -128

    describe "From Int8 Int" $ do
      let f = Witch.from @Int.Int8 @Int
      it "works" $ do
        f 0 `shouldBe` 0
        f 127 `shouldBe` 127
        f -128 `shouldBe` -128

    describe "From Int8 Integer" $ do
      let f = Witch.from @Int.Int8 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 127 `shouldBe` 127
        f -128 `shouldBe` -128

    describe "TryFrom Int8 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f -1 `shouldBe` Nothing

    describe "TryFrom Int8 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f -1 `shouldBe` Nothing

    describe "TryFrom Int8 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f -1 `shouldBe` Nothing

    describe "TryFrom Int8 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f -1 `shouldBe` Nothing

    describe "TryFrom Int8 Word" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f -1 `shouldBe` Nothing

    describe "TryFrom Int8 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int8 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f -1 `shouldBe` Nothing

    describe "From Int8 Float" $ do
      let f = Witch.from @Int.Int8 @Float
      it "works" $ do
        f 0 `shouldBe` 0
        f 127 `shouldBe` 127
        f -128 `shouldBe` -128

    describe "From Int8 Double" $ do
      let f = Witch.from @Int.Int8 @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 127 `shouldBe` 127
        f -128 `shouldBe` -128

    describe "TryFrom Int16 Int8" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing
        f -128 `shouldBe` Just -128
        f -129 `shouldBe` Nothing

    describe "From Int16 Int32" $ do
      let f = Witch.from @Int.Int16 @Int.Int32
      it "works" $ do
        f 0 `shouldBe` 0
        f 32767 `shouldBe` 32767
        f -32768 `shouldBe` -32768

    describe "From Int16 Int64" $ do
      let f = Witch.from @Int.Int16 @Int.Int64
      it "works" $ do
        f 0 `shouldBe` 0
        f 32767 `shouldBe` 32767
        f -32768 `shouldBe` -32768

    describe "From Int16 Int" $ do
      let f = Witch.from @Int.Int16 @Int
      it "works" $ do
        f 0 `shouldBe` 0
        f 32767 `shouldBe` 32767
        f -32768 `shouldBe` -32768

    describe "From Int16 Integer" $ do
      let f = Witch.from @Int.Int16 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 32767 `shouldBe` 32767
        f -32768 `shouldBe` -32768

    describe "TryFrom Int16 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Int16 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f -1 `shouldBe` Nothing

    describe "TryFrom Int16 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f -1 `shouldBe` Nothing

    describe "TryFrom Int16 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f -1 `shouldBe` Nothing

    describe "TryFrom Int16 Word" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f -1 `shouldBe` Nothing

    describe "TryFrom Int16 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int16 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f -1 `shouldBe` Nothing

    describe "From Int16 Float" $ do
      let f = Witch.from @Int.Int16 @Float
      it "works" $ do
        f 0 `shouldBe` 0
        f 32767 `shouldBe` 32767
        f -32768 `shouldBe` -32768

    describe "From Int16 Double" $ do
      let f = Witch.from @Int.Int16 @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 32767 `shouldBe` 32767
        f -32768 `shouldBe` -32768

    describe "TryFrom Int32 Int8" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing
        f -128 `shouldBe` Just -128
        f -129 `shouldBe` Nothing

    describe "TryFrom Int32 Int16" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing
        f -32768 `shouldBe` Just -32768
        f -32769 `shouldBe` Nothing

    describe "From Int32 Int64" $ do
      let f = Witch.from @Int.Int32 @Int.Int64
      it "works" $ do
        f 0 `shouldBe` 0
        f 2147483647 `shouldBe` 2147483647
        f -2147483648 `shouldBe` -2147483648

    describe "TryFrom Int32 Int" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f -2147483648 `shouldBe` Just -2147483648

    describe "From Int32 Integer" $ do
      let f = Witch.from @Int.Int32 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 2147483647 `shouldBe` 2147483647
        f -2147483648 `shouldBe` -2147483648

    describe "TryFrom Int32 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Int32 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Int32 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f -1 `shouldBe` Nothing

    describe "TryFrom Int32 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f -1 `shouldBe` Nothing

    describe "TryFrom Int32 Word" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f -1 `shouldBe` Nothing

    describe "TryFrom Int32 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f -1 `shouldBe` Nothing

    describe "TryFrom Int32 Float" $ do
      let f = hush . Witch.tryFrom @Int.Int32 @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing

    describe "From Int32 Double" $ do
      let f = Witch.from @Int.Int32 @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 2147483647 `shouldBe` 2147483647
        f -2147483648 `shouldBe` -2147483648

    describe "TryFrom Int64 Int8" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing
        f -128 `shouldBe` Just -128
        f -129 `shouldBe` Nothing

    describe "TryFrom Int64 Int16" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing
        f -32768 `shouldBe` Just -32768
        f -32769 `shouldBe` Nothing

    describe "TryFrom Int64 Int32" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f 2147483648 `shouldBe` Nothing
        f -2147483648 `shouldBe` Just -2147483648
        f -2147483649 `shouldBe` Nothing

    describe "TryFrom Int64 Int" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Int
          hi = maxBound :: Int
          lo = minBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 9223372036854775807
          then f 9223372036854775807 `shouldBe` Just 9223372036854775807
          else f (fromIntegral hi) `shouldBe` Just hi
        if toInteger lo <= -9223372036854775808
          then f -9223372036854775808 `shouldBe` Just -9223372036854775808
          else f (fromIntegral lo) `shouldBe` Just lo

    describe "From Int64 Integer" $ do
      let f = Witch.from @Int.Int64 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 9223372036854775807 `shouldBe` 9223372036854775807
        f -9223372036854775808 `shouldBe` -9223372036854775808

    describe "TryFrom Int64 Word8" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Int64 Word16" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Int64 Word32" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f -1 `shouldBe` Nothing

    describe "TryFrom Int64 Word64" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9223372036854775807 `shouldBe` Just 9223372036854775807
        f -1 `shouldBe` Nothing

    describe "TryFrom Int64 Word" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Word
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 9223372036854775807
          then f 9223372036854775807 `shouldBe` Just 9223372036854775807
          else f (fromIntegral hi) `shouldBe` Just hi
        f -1 `shouldBe` Nothing

    describe "TryFrom Int64 Natural" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9223372036854775807 `shouldBe` Just 9223372036854775807
        f -1 `shouldBe` Nothing

    describe "TryFrom Int64 Float" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing

    describe "TryFrom Int64 Double" $ do
      let f = hush . Witch.tryFrom @Int.Int64 @Double
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing
        f -9007199254740991 `shouldBe` Just -9007199254740991
        f -9007199254740992 `shouldBe` Nothing

    describe "TryFrom Int Int8" $ do
      let f = hush . Witch.tryFrom @Int @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing
        f -128 `shouldBe` Just -128
        f -129 `shouldBe` Nothing

    describe "TryFrom Int Int16" $ do
      let f = hush . Witch.tryFrom @Int @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing
        f -32768 `shouldBe` Just -32768
        f -32769 `shouldBe` Nothing

    describe "TryFrom Int Int32" $ do
      let f = hush . Witch.tryFrom @Int @Int.Int32
          hi = maxBound :: Int
          lo = minBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        if toInteger hi >= 2147483648
          then f 2147483648 `shouldBe` Nothing
          else f hi `shouldBe` Just (fromIntegral hi)
        f -2147483648 `shouldBe` Just -2147483648
        if toInteger lo <= -2147483649
          then f -2147483649 `shouldBe` Nothing
          else f lo `shouldBe` Just (fromIntegral lo)

    describe "From Int Int64" $ do
      let f = Witch.from @Int @Int.Int64
      it "works" $ do
        f 0 `shouldBe` 0
        f maxBound `shouldBe` fromIntegral (maxBound :: Int)
        f minBound `shouldBe` fromIntegral (minBound :: Int)

    describe "From Int Integer" $ do
      let f = Witch.from @Int @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f maxBound `shouldBe` fromIntegral (maxBound :: Int)
        f minBound `shouldBe` fromIntegral (minBound :: Int)

    describe "TryFrom Int Word8" $ do
      let f = hush . Witch.tryFrom @Int @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Int Word16" $ do
      let f = hush . Witch.tryFrom @Int @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Int Word32" $ do
      let f = hush . Witch.tryFrom @Int @Word.Word32
          hi = maxBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 4294967295
          then f 4294967295 `shouldBe` Just 4294967295
          else f hi `shouldBe` Just (fromIntegral hi)
        if toInteger hi >= 4294967296
          then f 4294967296 `shouldBe` Nothing
          else f hi `shouldBe` Just (fromIntegral hi)
        f -1 `shouldBe` Nothing

    describe "TryFrom Int Word64" $ do
      let f = hush . Witch.tryFrom @Int @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f maxBound `shouldBe` Just (fromIntegral (maxBound :: Int))
        f -1 `shouldBe` Nothing

    describe "TryFrom Int Word" $ do
      let f = hush . Witch.tryFrom @Int @Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f maxBound `shouldBe` Just (fromIntegral (maxBound :: Int))
        f -1 `shouldBe` Nothing

    describe "TryFrom Int Natural" $ do
      let f = hush . Witch.tryFrom @Int @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f maxBound `shouldBe` Just (fromIntegral (maxBound :: Int))
        f -1 `shouldBe` Nothing

    describe "TryFrom Int Float" $ do
      let f = hush . Witch.tryFrom @Int @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing

    describe "TryFrom Int Double" $ do
      let f = hush . Witch.tryFrom @Int @Double
          hi = maxBound :: Int
          lo = minBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 9007199254740991
          then f 9007199254740991 `shouldBe` Just 9007199254740991
          else f hi `shouldBe` Just (fromIntegral hi)
        if toInteger hi >= 9007199254740992
          then f 9007199254740992 `shouldBe` Nothing
          else f hi `shouldBe` Just (fromIntegral hi)
        if toInteger lo <= -9007199254740991
          then f -9007199254740991 `shouldBe` Just -9007199254740991
          else f lo `shouldBe` Just (fromIntegral lo)
        if toInteger lo <= -9007199254740992
          then f -9007199254740992 `shouldBe` Nothing
          else f lo `shouldBe` Just (fromIntegral lo)

    describe "TryFrom Integer Int8" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing
        f -128 `shouldBe` Just -128
        f -129 `shouldBe` Nothing

    describe "TryFrom Integer Int16" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing
        f -32768 `shouldBe` Just -32768
        f -32769 `shouldBe` Nothing

    describe "TryFrom Integer Int32" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f 2147483648 `shouldBe` Nothing
        f -2147483648 `shouldBe` Just -2147483648
        f -2147483649 `shouldBe` Nothing

    describe "TryFrom Integer Int64" $ do
      let f = hush . Witch.tryFrom @Integer @Int.Int64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9223372036854775807 `shouldBe` Just 9223372036854775807
        f 9223372036854775808 `shouldBe` Nothing
        f -9223372036854775808 `shouldBe` Just -9223372036854775808
        f -9223372036854775809 `shouldBe` Nothing

    describe "TryFrom Integer Int" $ do
      let f = hush . Witch.tryFrom @Integer @Int
          hi = maxBound :: Int
          lo = minBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f (fromIntegral hi) `shouldBe` Just hi
        f (toInteger hi + 1) `shouldBe` Nothing
        f (fromIntegral lo) `shouldBe` Just lo
        f (toInteger lo - 1) `shouldBe` Nothing

    describe "TryFrom Integer Word8" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Integer Word16" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Integer Word32" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 4294967295 `shouldBe` Just 4294967295
        f 4294967296 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Integer Word64" $ do
      let f = hush . Witch.tryFrom @Integer @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 18446744073709551615 `shouldBe` Just 18446744073709551615
        f 18446744073709551616 `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Integer Word" $ do
      let f = hush . Witch.tryFrom @Integer @Word
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f (fromIntegral hi) `shouldBe` Just hi
        f (toInteger hi + 1) `shouldBe` Nothing
        f -1 `shouldBe` Nothing

    describe "TryFrom Integer Natural" $ do
      let f = hush . Witch.tryFrom @Integer @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 18446744073709551616 `shouldBe` Just 18446744073709551616
        f -1 `shouldBe` Nothing

    describe "TryFrom Integer Float" $ do
      let f = hush . Witch.tryFrom @Integer @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing

    describe "TryFrom Integer Double" $ do
      let f = hush . Witch.tryFrom @Integer @Double
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing
        f -9007199254740991 `shouldBe` Just -9007199254740991
        f -9007199254740992 `shouldBe` Nothing

    describe "From Word8 Word16" $ do
      let f = Witch.from @Word.Word8 @Word.Word16
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Word32" $ do
      let f = Witch.from @Word.Word8 @Word.Word32
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Word64" $ do
      let f = Witch.from @Word.Word8 @Word.Word64
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Word" $ do
      let f = Witch.from @Word.Word8 @Word
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Natural" $ do
      let f = Witch.from @Word.Word8 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "TryFrom Word8 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word8 @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing

    describe "From Word8 Int16" $ do
      let f = Witch.from @Word.Word8 @Int.Int16
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Int32" $ do
      let f = Witch.from @Word.Word8 @Int.Int32
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Int64" $ do
      let f = Witch.from @Word.Word8 @Int.Int64
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Int" $ do
      let f = Witch.from @Word.Word8 @Int
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Integer" $ do
      let f = Witch.from @Word.Word8 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Float" $ do
      let f = Witch.from @Word.Word8 @Float
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "From Word8 Double" $ do
      let f = Witch.from @Word.Word8 @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 255 `shouldBe` 255

    describe "TryFrom Word16 Word8" $ do
      let f = hush . Witch.tryFrom @Word.Word16 @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing

    describe "From Word16 Word32" $ do
      let f = Witch.from @Word.Word16 @Word.Word32
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Word64" $ do
      let f = Witch.from @Word.Word16 @Word.Word64
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Word" $ do
      let f = Witch.from @Word.Word16 @Word
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Natural" $ do
      let f = Witch.from @Word.Word16 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "TryFrom Word16 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word16 @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing

    describe "TryFrom Word16 Int16" $ do
      let f = hush . Witch.tryFrom @Word.Word16 @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing

    describe "From Word16 Int32" $ do
      let f = Witch.from @Word.Word16 @Int.Int32
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Int64" $ do
      let f = Witch.from @Word.Word16 @Int.Int64
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Int" $ do
      let f = Witch.from @Word.Word16 @Int
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Integer" $ do
      let f = Witch.from @Word.Word16 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Float" $ do
      let f = Witch.from @Word.Word16 @Float
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "From Word16 Double" $ do
      let f = Witch.from @Word.Word16 @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 65535 `shouldBe` 65535

    describe "TryFrom Word32 Word8" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing

    describe "TryFrom Word32 Word16" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing

    describe "From Word32 Word64" $ do
      let f = Witch.from @Word.Word32 @Word.Word64
      it "works" $ do
        f 0 `shouldBe` 0
        f 4294967295 `shouldBe` 4294967295

    describe "TryFrom Word32 Word" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 4294967295 `shouldBe` Just 4294967295

    describe "From Word32 Natural" $ do
      let f = Witch.from @Word.Word32 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` 0
        f 4294967295 `shouldBe` 4294967295

    describe "TryFrom Word32 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing

    describe "TryFrom Word32 Int16" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing

    describe "TryFrom Word32 Int32" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f 2147483648 `shouldBe` Nothing

    describe "From Word32 Int64" $ do
      let f = Witch.from @Word.Word32 @Int.Int64
      it "works" $ do
        f 0 `shouldBe` 0
        f 4294967295 `shouldBe` 4294967295

    describe "TryFrom Word32 Int" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Int
          hi = maxBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 4294967295
          then f 4294967295 `shouldBe` Just 4294967295
          else f (fromIntegral hi) `shouldBe` Just hi

    describe "From Word32 Integer" $ do
      let f = Witch.from @Word.Word32 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 4294967295 `shouldBe` 4294967295

    describe "TryFrom Word32 Float" $ do
      let f = hush . Witch.tryFrom @Word.Word32 @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing

    describe "From Word32 Double" $ do
      let f = Witch.from @Word.Word32 @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 4294967295 `shouldBe` 4294967295

    describe "TryFrom Word64 Word8" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing

    describe "TryFrom Word64 Word16" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing

    describe "TryFrom Word64 Word32" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 4294967295 `shouldBe` Just 4294967295
        f 4294967296 `shouldBe` Nothing

    describe "TryFrom Word64 Word" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Word
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 18446744073709551615
          then f 18446744073709551615 `shouldBe` Just 18446744073709551615
          else f (fromIntegral hi) `shouldBe` Just hi

    describe "From Word64 Natural" $ do
      let f = Witch.from @Word.Word64 @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` 0
        f 18446744073709551615 `shouldBe` 18446744073709551615

    describe "TryFrom Word64 Int8" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing

    describe "TryFrom Word64 Int16" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing

    describe "TryFrom Word64 Int32" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f 2147483648 `shouldBe` Nothing

    describe "TryFrom Word64 Int64" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9223372036854775807 `shouldBe` Just 9223372036854775807
        f 9223372036854775808 `shouldBe` Nothing

    describe "TryFrom Word64 Int" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Int
          hi = maxBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f (fromIntegral hi) `shouldBe` Just hi
        f (fromIntegral hi + 1) `shouldBe` Nothing

    describe "From Word64 Integer" $ do
      let f = Witch.from @Word.Word64 @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 18446744073709551615 `shouldBe` 18446744073709551615

    describe "TryFrom Word64 Float" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing

    describe "TryFrom Word64 Double" $ do
      let f = hush . Witch.tryFrom @Word.Word64 @Double
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing

    describe "TryFrom Word Word8" $ do
      let f = hush . Witch.tryFrom @Word @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing

    describe "TryFrom Word Word16" $ do
      let f = hush . Witch.tryFrom @Word @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing

    describe "TryFrom Word Word32" $ do
      let f = hush . Witch.tryFrom @Word @Word.Word32
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 4294967295 `shouldBe` Just 4294967295
        if toInteger hi >= 4294967296
          then f 4294967296 `shouldBe` Nothing
          else f hi `shouldBe` Just (fromIntegral hi)

    describe "From Word Word64" $ do
      let f = Witch.from @Word @Word.Word64
      it "works" $ do
        f 0 `shouldBe` 0
        f maxBound `shouldBe` fromIntegral (maxBound :: Word)

    describe "From Word Natural" $ do
      let f = Witch.from @Word @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` 0
        f maxBound `shouldBe` fromIntegral (maxBound :: Word)

    describe "TryFrom Word Int8" $ do
      let f = hush . Witch.tryFrom @Word @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing

    describe "TryFrom Word Int16" $ do
      let f = hush . Witch.tryFrom @Word @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing

    describe "TryFrom Word Int32" $ do
      let f = hush . Witch.tryFrom @Word @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f 2147483648 `shouldBe` Nothing

    describe "TryFrom Word Int64" $ do
      let f = hush . Witch.tryFrom @Word @Int.Int64
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 9223372036854775807
          then f 9223372036854775807 `shouldBe` Just 9223372036854775807
          else f hi `shouldBe` Just (fromIntegral hi)
        if toInteger hi >= 9223372036854775808
          then f 9223372036854775808 `shouldBe` Nothing
          else f hi `shouldBe` Just (fromIntegral hi)

    describe "TryFrom Word Int" $ do
      let f = hush . Witch.tryFrom @Word @Int
          hi = maxBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f (fromIntegral hi) `shouldBe` Just hi
        f (fromIntegral hi + 1) `shouldBe` Nothing

    describe "From Word Integer" $ do
      let f = Witch.from @Word @Integer
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` 0
        f hi `shouldBe` fromIntegral hi

    describe "TryFrom Word Float" $ do
      let f = hush . Witch.tryFrom @Word @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing

    describe "TryFrom Word Double" $ do
      let f = hush . Witch.tryFrom @Word @Double
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 9007199254740991
          then f 9007199254740991 `shouldBe` Just 9007199254740991
          else f hi `shouldBe` Just (fromIntegral hi)
        if toInteger hi >= 9007199254740992
          then f 9007199254740992 `shouldBe` Nothing
          else f hi `shouldBe` Just (fromIntegral hi)

    describe "TryFrom Natural Word8" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing

    describe "TryFrom Natural Word16" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing

    describe "TryFrom Natural Word32" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 4294967295 `shouldBe` Just 4294967295
        f 4294967296 `shouldBe` Nothing

    describe "TryFrom Natural Word64" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 18446744073709551615 `shouldBe` Just 18446744073709551615
        f 18446744073709551616 `shouldBe` Nothing

    describe "TryFrom Natural Word" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Word
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f (fromIntegral hi) `shouldBe` Just hi
        f (fromIntegral hi + 1) `shouldBe` Nothing

    describe "TryFrom Natural Int8" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing

    describe "TryFrom Natural Int16" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing

    describe "TryFrom Natural Int32" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f 2147483648 `shouldBe` Nothing

    describe "TryFrom Natural Int64" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9223372036854775807 `shouldBe` Just 9223372036854775807
        f 9223372036854775808 `shouldBe` Nothing

    describe "TryFrom Natural Int" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Int
          hi = maxBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f (fromIntegral hi) `shouldBe` Just hi
        f (fromIntegral hi + 1) `shouldBe` Nothing

    describe "From Natural Integer" $ do
      let f = Witch.from @Natural.Natural @Integer
      it "works" $ do
        f 0 `shouldBe` 0
        f 9223372036854775808 `shouldBe` 9223372036854775808

    describe "TryFrom Natural Float" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing

    describe "TryFrom Natural Double" $ do
      let f = hush . Witch.tryFrom @Natural.Natural @Double
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing

    describe "TryFrom Float Int8" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing
        f -128 `shouldBe` Just -128
        f -129 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Int16" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing
        f -32768 `shouldBe` Just -32768
        f -32769 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Int32" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Int64" $ do
      let f = hush . Witch.tryFrom @Float @Int.Int64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Int" $ do
      let f = hush . Witch.tryFrom @Float @Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Integer" $ do
      let f = hush . Witch.tryFrom @Float @Integer
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f -16777215 `shouldBe` Just -16777215
        f -16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Word8" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Word16" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Word32" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Word64" $ do
      let f = hush . Witch.tryFrom @Float @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Word" $ do
      let f = hush . Witch.tryFrom @Float @Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Natural" $ do
      let f = hush . Witch.tryFrom @Float @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 16777215 `shouldBe` Just 16777215
        f 16777216 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Float Rational" $ do
      let f = hush . Witch.tryFrom @Float @Rational
      it "works" $ do
        f 0 `shouldBe` Just 0
        f -0 `shouldBe` Just 0
        f 0.5 `shouldBe` Just 0.5
        f (-0.5) `shouldBe` Just (-0.5)
        f 16777215 `shouldBe` Just 16777215
        f -16777215 `shouldBe` Just -16777215
        f 16777216 `shouldBe` Just 16777216
        f -16777216 `shouldBe` Just -16777216
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing
        f 0.1 `shouldBe` Just 0.1
        f (-0.1) `shouldBe` Just (-0.1)

    describe "From Float Double" $ do
      let f = Witch.from @Float @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 0.5 `shouldBe` 0.5
        f (-0.5) `shouldBe` (-0.5)
        f (0 / 0) `shouldSatisfy` isNaN
        f (1 / 0) `shouldBe` (1 / 0)
        f (-1 / 0) `shouldBe` (-1 / 0)

    describe "TryFrom Double Int8" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 127 `shouldBe` Just 127
        f 128 `shouldBe` Nothing
        f -128 `shouldBe` Just -128
        f -129 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Int16" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 32767 `shouldBe` Just 32767
        f 32768 `shouldBe` Nothing
        f -32768 `shouldBe` Just -32768
        f -32769 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Int32" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 2147483647 `shouldBe` Just 2147483647
        f 2147483648 `shouldBe` Nothing
        f -2147483648 `shouldBe` Just -2147483648
        f -2147483649 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Int64" $ do
      let f = hush . Witch.tryFrom @Double @Int.Int64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing
        f -9007199254740991 `shouldBe` Just -9007199254740991
        f -9007199254740992 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Int" $ do
      let f = hush . Witch.tryFrom @Double @Int
          hi = maxBound :: Int
          lo = minBound :: Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 9007199254740991
          then f 9007199254740991 `shouldBe` Just 9007199254740991
          else f (fromIntegral hi) `shouldBe` Just hi
        f 9007199254740992 `shouldBe` Nothing
        if toInteger lo <= -9007199254740991
          then f -9007199254740991 `shouldBe` Just -9007199254740991
          else f (fromIntegral lo) `shouldBe` Just lo
        f -9007199254740992 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Integer" $ do
      let f = hush . Witch.tryFrom @Double @Integer
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing
        f -9007199254740991 `shouldBe` Just -9007199254740991
        f -9007199254740992 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Word8" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word8
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 255 `shouldBe` Just 255
        f 256 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Word16" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word16
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 65535 `shouldBe` Just 65535
        f 65536 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Word32" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word32
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 4294967295 `shouldBe` Just 4294967295
        f 4294967296 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Word64" $ do
      let f = hush . Witch.tryFrom @Double @Word.Word64
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Word" $ do
      let f = hush . Witch.tryFrom @Double @Word
          hi = maxBound :: Word
      it "works" $ do
        f 0 `shouldBe` Just 0
        if toInteger hi >= 9007199254740991
          then f 9007199254740991 `shouldBe` Just 9007199254740991
          else f (fromIntegral hi) `shouldBe` Just hi
        f 9007199254740992 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Natural" $ do
      let f = hush . Witch.tryFrom @Double @Natural.Natural
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f 9007199254740992 `shouldBe` Nothing
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing

    describe "TryFrom Double Rational" $ do
      let f = hush . Witch.tryFrom @Double @Rational
      it "works" $ do
        f 0 `shouldBe` Just 0
        f -0 `shouldBe` Just 0
        f 0.5 `shouldBe` Just 0.5
        f (-0.5) `shouldBe` Just (-0.5)
        f 9007199254740991 `shouldBe` Just 9007199254740991
        f -9007199254740991 `shouldBe` Just -9007199254740991
        f 9007199254740992 `shouldBe` Just 9007199254740992
        f -9007199254740992 `shouldBe` Just -9007199254740992
        f (0 / 0) `shouldBe` Nothing
        f (1 / 0) `shouldBe` Nothing
        f (-1 / 0) `shouldBe` Nothing
        f 0.1 `shouldBe` Just 0.1
        f (-0.1) `shouldBe` Just (-0.1)

    describe "From Double Float" $ do
      let f = Witch.from @Double @Float
      it "works" $ do
        f 0 `shouldBe` 0
        f 0.5 `shouldBe` 0.5
        f (-0.5) `shouldBe` (-0.5)
        f (0 / 0) `shouldSatisfy` isNaN
        f (1 / 0) `shouldBe` (1 / 0)
        f (-1 / 0) `shouldBe` (-1 / 0)

    describe "From a (Ratio a)" $ do
      let f = Witch.from @Int @(Ratio.Ratio Int)
      it "works" $ do
        f 0 `shouldBe` 0
        f 1 `shouldBe` 1

    describe "TryFrom (Ratio a) a" $ do
      let f = hush . Witch.tryFrom @(Ratio.Ratio Int) @Int
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 0.5 `shouldBe` Nothing
        f 1 `shouldBe` Just 1

    describe "From Rational Float" $ do
      let f = Witch.from @Rational @Float
      it "works" $ do
        f 0 `shouldBe` 0
        f 0.5 `shouldBe` 0.5
        f (-0.5) `shouldBe` (-0.5)
        f 0.1 `shouldBe` 0.1
        f (-0.1) `shouldBe` (-0.1)

    describe "From Rational Double" $ do
      let f = Witch.from @Rational @Double
      it "works" $ do
        f 0 `shouldBe` 0
        f 0.5 `shouldBe` 0.5
        f (-0.5) `shouldBe` (-0.5)
        f 0.1 `shouldBe` 0.1
        f (-0.1) `shouldBe` (-0.1)

    describe "TryFrom Rational (Fixed a)" $ do
      let f = hush . Witch.tryFrom @Rational @Fixed.Deci
      it "works" $ do
        f 0.1 `shouldBe` Just 0.1
        f 1.2 `shouldBe` Just 1.2
        f 12.3 `shouldBe` Just 12.3
        f 0.12 `shouldBe` Nothing

    describe "From Integer (Fixed a)" $ do
      let f = Witch.from @Integer @Fixed.Deci
      it "works" $ do
        f 1 `shouldBe` 0.1
        f 10 `shouldBe` 1
        f 120 `shouldBe` 12

    describe "From (Fixed a) Integer" $ do
      let f = Witch.from @Fixed.Deci @Integer
      it "works" $ do
        f 0.1 `shouldBe` 1
        f 1 `shouldBe` 10
        f 12 `shouldBe` 120

    describe "From (Fixed a) Rational" $ do
      let f = Witch.from @Fixed.Deci @Rational
      it "works" $ do
        f 0.1 `shouldBe` 0.1
        f 1 `shouldBe` 1
        f 12 `shouldBe` 12

    describe "From a (Complex a)" $ do
      let f = Witch.from @Float @(Complex.Complex Float)
      it "works" $ do
        f 0 `shouldBe` 0
        f 1 `shouldBe` 1

    describe "TryFrom (Complex a) a" $ do
      let f = hush . Witch.tryFrom @(Complex.Complex Float) @Float
      it "works" $ do
        f 0 `shouldBe` Just 0
        f 1 `shouldBe` Just 1
        f (0 Complex.:+ 1) `shouldBe` Nothing

    describe "TryFrom [a] (NonEmpty a)" $ do
      let f = hush . Witch.tryFrom @[Int] @(NonEmpty.NonEmpty Int)
      it "works" $ do
        f [] `shouldBe` Nothing
        f [1] `shouldBe` Just (1 NonEmpty.:| [])
        f [1, 2] `shouldBe` Just (1 NonEmpty.:| [2])

    describe "From (NonEmpty a) [a]" $ do
      let f = Witch.from @(NonEmpty.NonEmpty Int) @[Int]
      it "works" $ do
        f (1 NonEmpty.:| []) `shouldBe` [1]
        f (1 NonEmpty.:| [2]) `shouldBe` [1, 2]

    describe "From [a] (Set a)" $ do
      let f = Witch.from @[Char] @(Set.Set Char)
      it "works" $ do
        f [] `shouldBe` Set.fromList []
        f ['a'] `shouldBe` Set.fromList ['a']
        f ['a', 'b'] `shouldBe` Set.fromList ['a', 'b']
        f ['a', 'a'] `shouldBe` Set.fromList ['a']

    describe "From (Set a) [a]" $ do
      let f = Witch.from @(Set.Set Char) @[Char]
      it "works" $ do
        f (Set.fromList []) `shouldBe` []
        f (Set.fromList ['a']) `shouldBe` ['a']
        f (Set.fromList ['a', 'b']) `shouldBe` ['a', 'b']

    describe "From [Int] IntSet" $ do
      let f = Witch.from @[Int] @IntSet.IntSet
      it "works" $ do
        f [] `shouldBe` IntSet.fromList []
        f [1] `shouldBe` IntSet.fromList [1]
        f [1, 2] `shouldBe` IntSet.fromList [1, 2]

    describe "From IntSet [Int]" $ do
      let f = Witch.from @IntSet.IntSet @[Int]
      it "works" $ do
        f (IntSet.fromList []) `shouldBe` []
        f (IntSet.fromList [1]) `shouldBe` [1]
        f (IntSet.fromList [1, 2]) `shouldBe` [1, 2]

    describe "From [(k, v)] (Map k v)" $ do
      let f = Witch.from @[(Char, Int)] @(Map.Map Char Int)
      it "works" $ do
        f [] `shouldBe` Map.empty
        f [('a', 1)] `shouldBe` Map.fromList [('a', 1)]
        f [('a', 1), ('b', 2)] `shouldBe` Map.fromList [('a', 1), ('b', 2)]
        f [('a', 1), ('a', 2)] `shouldBe` Map.fromList [('a', 2)]

    describe "From (Map k v) [(k, v)]" $ do
      let f = Witch.from @(Map.Map Char Int) @[(Char, Int)]
      it "works" $ do
        f Map.empty `shouldBe` []
        f (Map.fromList [('a', 1)]) `shouldBe` [('a', 1)]
        f (Map.fromList [('a', 1), ('b', 2)]) `shouldBe` [('a', 1), ('b', 2)]

    describe "From [(Int, v)] (IntMap v)" $ do
      let f = Witch.from @[(Int, Char)] @(IntMap.IntMap Char)
      it "works" $ do
        f [] `shouldBe` IntMap.fromList []
        f [(1, 'a')] `shouldBe` IntMap.fromList [(1, 'a')]
        f [(1, 'a'), (2, 'b')] `shouldBe` IntMap.fromList [(1, 'a'), (2, 'b')]
        f [(1, 'a'), (1, 'b')] `shouldBe` IntMap.fromList [(1, 'b')]

    describe "From (IntMap v) [(Int, v)]" $ do
      let f = Witch.from @(IntMap.IntMap Char) @[(Int, Char)]
      it "works" $ do
        f (IntMap.fromList []) `shouldBe` []
        f (IntMap.fromList [(1, 'a')]) `shouldBe` [(1, 'a')]
        f (IntMap.fromList [(1, 'a'), (2, 'b')]) `shouldBe` [(1, 'a'), (2, 'b')]

    describe "From [a] (Seq a)" $ do
      let f = Witch.from @[Int] @(Seq.Seq Int)
      it "works" $ do
        f [] `shouldBe` Seq.fromList []
        f [1] `shouldBe` Seq.fromList [1]
        f [1, 2] `shouldBe` Seq.fromList [1, 2]

    describe "From (Seq a) [a]" $ do
      let f = Witch.from @(Seq.Seq Int) @[Int]
      it "works" $ do
        f (Seq.fromList []) `shouldBe` []
        f (Seq.fromList [1]) `shouldBe` [1]
        f (Seq.fromList [1, 2]) `shouldBe` [1, 2]

    describe "From [Word8] ByteString" $ do
      let f = Witch.from @[Word.Word8] @ByteString.ByteString
      it "works" $ do
        f [] `shouldBe` ByteString.pack []
        f [0x00] `shouldBe` ByteString.pack [0x00]
        f [0x0f, 0xf0] `shouldBe` ByteString.pack [0x0f, 0xf0]

    describe "From ByteString [Word8]" $ do
      let f = Witch.from @ByteString.ByteString @[Word.Word8]
      it "works" $ do
        f (ByteString.pack []) `shouldBe` []
        f (ByteString.pack [0x00]) `shouldBe` [0x00]
        f (ByteString.pack [0x0f, 0xf0]) `shouldBe` [0x0f, 0xf0]

    describe "From ByteString LazyByteString" $ do
      let f = Witch.from @ByteString.ByteString @LazyByteString.ByteString
      it "works" $ do
        f (ByteString.pack []) `shouldBe` LazyByteString.pack []
        f (ByteString.pack [0x00]) `shouldBe` LazyByteString.pack [0x00]
        f (ByteString.pack [0x0f, 0xf0]) `shouldBe` LazyByteString.pack [0x0f, 0xf0]

    describe "From ByteString ShortByteString" $ do
      let f = Witch.from @ByteString.ByteString @ShortByteString.ShortByteString
      it "works" $ do
        f (ByteString.pack []) `shouldBe` ShortByteString.pack []
        f (ByteString.pack [0x00]) `shouldBe` ShortByteString.pack [0x00]
        f (ByteString.pack [0x0f, 0xf0]) `shouldBe` ShortByteString.pack [0x0f, 0xf0]

    describe "From [Word8] LazyByteString" $ do
      let f = Witch.from @[Word.Word8] @LazyByteString.ByteString
      it "works" $ do
        f [] `shouldBe` LazyByteString.pack []
        f [0x00] `shouldBe` LazyByteString.pack [0x00]
        f [0x0f, 0xf0] `shouldBe` LazyByteString.pack [0x0f, 0xf0]

    describe "From LazyByteString [Word8]" $ do
      let f = Witch.from @LazyByteString.ByteString @[Word.Word8]
      it "works" $ do
        f (LazyByteString.pack []) `shouldBe` []
        f (LazyByteString.pack [0x00]) `shouldBe` [0x00]
        f (LazyByteString.pack [0x0f, 0xf0]) `shouldBe` [0x0f, 0xf0]

    describe "From LazyByteString ByteString" $ do
      let f = Witch.from @LazyByteString.ByteString @ByteString.ByteString
      it "works" $ do
        f (LazyByteString.pack []) `shouldBe` ByteString.pack []
        f (LazyByteString.pack [0x00]) `shouldBe` ByteString.pack [0x00]
        f (LazyByteString.pack [0x0f, 0xf0]) `shouldBe` ByteString.pack [0x0f, 0xf0]

    describe "From [Word8] ShortByteString" $ do
      let f = Witch.from @[Word.Word8] @ShortByteString.ShortByteString
      it "works" $ do
        f [] `shouldBe` ShortByteString.pack []
        f [0x00] `shouldBe` ShortByteString.pack [0x00]
        f [0x0f, 0xf0] `shouldBe` ShortByteString.pack [0x0f, 0xf0]

    describe "From ShortByteString [Word8]" $ do
      let f = Witch.from @ShortByteString.ShortByteString @[Word.Word8]
      it "works" $ do
        f (ShortByteString.pack []) `shouldBe` []
        f (ShortByteString.pack [0x00]) `shouldBe` [0x00]
        f (ShortByteString.pack [0x0f, 0xf0]) `shouldBe` [0x0f, 0xf0]

    describe "From ShortByteString ByteString" $ do
      let f = Witch.from @ShortByteString.ShortByteString @ByteString.ByteString
      it "works" $ do
        f (ShortByteString.pack []) `shouldBe` ByteString.pack []
        f (ShortByteString.pack [0x00]) `shouldBe` ByteString.pack [0x00]
        f (ShortByteString.pack [0x0f, 0xf0]) `shouldBe` ByteString.pack [0x0f, 0xf0]

    describe "From Text LazyText" $ do
      let f = Witch.from @Text.Text @LazyText.Text
      it "works" $ do
        f (Text.pack "") `shouldBe` LazyText.pack ""
        f (Text.pack "a") `shouldBe` LazyText.pack "a"
        f (Text.pack "ab") `shouldBe` LazyText.pack "ab"

    describe "From LazyText Text" $ do
      let f = Witch.from @LazyText.Text @Text.Text
      it "works" $ do
        f (LazyText.pack "") `shouldBe` Text.pack ""
        f (LazyText.pack "a") `shouldBe` Text.pack "a"
        f (LazyText.pack "ab") `shouldBe` Text.pack "ab"

    describe "From String Text" $ do
      let f = Witch.from @String @Text.Text
      it "works" $ do
        f "" `shouldBe` Text.pack ""
        f "a" `shouldBe` Text.pack "a"
        f "ab" `shouldBe` Text.pack "ab"

    describe "From Text String" $ do
      let f = Witch.from @Text.Text @String
      it "works" $ do
        f (Text.pack "") `shouldBe` ""
        f (Text.pack "a") `shouldBe` "a"
        f (Text.pack "ab") `shouldBe` "ab"

    describe "From String LazyText" $ do
      let f = Witch.from @String @LazyText.Text
      it "works" $ do
        f "" `shouldBe` LazyText.pack ""
        f "a" `shouldBe` LazyText.pack "a"
        f "ab" `shouldBe` LazyText.pack "ab"

    describe "From LazyText String" $ do
      let f = Witch.from @LazyText.Text @String
      it "works" $ do
        f (LazyText.pack "") `shouldBe` ""
        f (LazyText.pack "a") `shouldBe` "a"
        f (LazyText.pack "ab") `shouldBe` "ab"

    describe "From Integer Day" $ do
      let f = Witch.from @Integer @Time.Day
      it "works" $ do
        f 0 `shouldBe` Time.ModifiedJulianDay 0

    describe "From Day Integer" $ do
      let f = Witch.from @Time.Day @Integer
      it "works" $ do
        f (Time.ModifiedJulianDay 0) `shouldBe` 0

    describe "From Day DayOfWeek" $ do
      let f = Witch.from @Time.Day @Time.DayOfWeek
      it "works" $ do
        f (Time.ModifiedJulianDay 0) `shouldBe` Time.Wednesday

    describe "From Rational UniversalTime" $ do
      let f = Witch.from @Rational @Time.UniversalTime
      it "works" $ do
        f 0 `shouldBe` Time.ModJulianDate 0

    describe "From UniversalTime Rational" $ do
      let f = Witch.from @Time.UniversalTime @Rational
      it "works" $ do
        f (Time.ModJulianDate 0) `shouldBe` 0

    describe "From Pico DiffTime" $ do
      let f = Witch.from @Fixed.Pico @Time.DiffTime
      it "works" $ do
        f 0 `shouldBe` 0

    describe "From DiffTime Pico" $ do
      let f = Witch.from @Time.DiffTime @Fixed.Pico
      it "works" $ do
        f 0 `shouldBe` 0

    describe "From Pico NominalDiffTime" $ do
      let f = Witch.from @Fixed.Pico @Time.NominalDiffTime
      it "works" $ do
        f 0 `shouldBe` 0

    describe "From NominalDiffTime Pico" $ do
      let f = Witch.from @Time.NominalDiffTime @Fixed.Pico
      it "works" $ do
        f 0 `shouldBe` 0

    describe "From SystemTime POSIXTime" $ do
      let f = Witch.from @Time.SystemTime @Time.POSIXTime
      it "works" $ do
        f (Time.MkSystemTime 0 0) `shouldBe` 0

    describe "From UTCTime POSIXTime" $ do
      let f = Witch.from @Time.UTCTime @Time.POSIXTime
      it "works" $ do
        f unixEpoch `shouldBe` 0

    describe "From POSIXTime UTCTime" $ do
      let f = Witch.from @Time.POSIXTime @Time.UTCTime
      it "works" $ do
        f 0 `shouldBe` unixEpoch

    describe "From UTCTime SystemTime" $ do
      let f = Witch.from @Time.UTCTime @Time.SystemTime
      it "works" $ do
        f unixEpoch `shouldBe` Time.MkSystemTime 0 0

    describe "From SystemTime AbsoluteTime" $ do
      let f = Witch.from @Time.SystemTime @Time.AbsoluteTime
      it "works" $ do
        f (Time.MkSystemTime -3506716800 0) `shouldBe` Time.taiEpoch

    describe "From SystemTime UTCTime" $ do
      let f = Witch.from @Time.SystemTime @Time.UTCTime
      it "works" $ do
        f (Time.MkSystemTime 0 0) `shouldBe` unixEpoch

    describe "From DiffTime TimeOfDay" $ do
      let f = Witch.from @Time.DiffTime @Time.TimeOfDay
      it "works" $ do
        f 0 `shouldBe` Time.TimeOfDay 0 0 0

    describe "From Rational TimeOfDay" $ do
      let f = Witch.from @Rational @Time.TimeOfDay
      it "works" $ do
        f 0 `shouldBe` Time.TimeOfDay 0 0 0

    describe "From TimeOfDay DiffTime" $ do
      let f = Witch.from @Time.TimeOfDay @Time.DiffTime
      it "works" $ do
        f (Time.TimeOfDay 0 0 0) `shouldBe` 0

    describe "From TimeOfDay Rational" $ do
      let f = Witch.from @Time.TimeOfDay @Rational
      it "works" $ do
        f (Time.TimeOfDay 0 0 0) `shouldBe` 0

    describe "From CalendarDiffDays CalendarDiffTime" $ do
      let f = Witch.from @Time.CalendarDiffDays @Time.CalendarDiffTime
      it "works" $ do
        f (Time.CalendarDiffDays 0 0) `shouldBe` Time.CalendarDiffTime 0 0

    describe "From NominalDiffTime CalendarDiffTime" $ do
      let f = Witch.from @Time.NominalDiffTime @Time.CalendarDiffTime
      it "works" $ do
        f 0 `shouldBe` Time.CalendarDiffTime 0 0

    describe "From ZonedTime UTCTime" $ do
      let f = Witch.from @Time.ZonedTime @Time.UTCTime
      it "works" $ do
        f (Time.ZonedTime (Time.LocalTime (Time.ModifiedJulianDay 0) (Time.TimeOfDay 0 0 0)) Time.utc) `shouldBe` Time.UTCTime (Time.ModifiedJulianDay 0) 0

    describe "From a (Tagged t a)" $ do
      let f = Witch.from @Bool @(Tagged.Tagged () Bool)
      it "works" $ do
        f False `shouldBe` Tagged.Tagged False

    describe "From (Tagged t a) a" $ do
      let f = Witch.from @(Tagged.Tagged () Bool) @Bool
      it "works" $ do
        f (Tagged.Tagged False) `shouldBe` False

    describe "From (Tagged t a) (Tagged u a)" $ do
      let f = Witch.from @(Tagged.Tagged "old" Bool) @(Tagged.Tagged "new" Bool)
      it "works" $ do
        f (Tagged.Tagged False) `shouldBe` Tagged.Tagged False

    describe "From Latin1S Text" $ do
      let f = Witch.from @Encoding.Latin1S @Text.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x61])) `shouldBe` Text.pack "a"

    describe "From Latin1S LazyText" $ do
      let f = Witch.from @Encoding.Latin1S @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x61])) `shouldBe` LazyText.pack "a"

    describe "From Latin1S String" $ do
      let f = Witch.from @Encoding.Latin1S @String
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x61])) `shouldBe` "a"

    describe "From Latin1L LazyText" $ do
      let f = Witch.from @Encoding.Latin1L @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61])) `shouldBe` LazyText.pack "a"

    describe "From Latin1L Text" $ do
      let f = Witch.from @Encoding.Latin1L @Text.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61])) `shouldBe` Text.pack "a"

    describe "From Latin1L String" $ do
      let f = Witch.from @Encoding.Latin1L @String
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61])) `shouldBe` "a"

    describe "TryFrom Text Latin1S" $ do
      let f = hush . Witch.tryFrom @Text.Text @Encoding.Latin1S
      it "works" $ do
        f (Text.pack "a") `shouldBe` Just (Tagged.Tagged $ ByteString.pack [0x61])
        f (Text.pack "\x100") `shouldBe` Nothing

    describe "TryFrom Text Latin1L" $ do
      let f = hush . Witch.tryFrom @Text.Text @Encoding.Latin1L
      it "works" $ do
        f (Text.pack "a") `shouldBe` Just (Tagged.Tagged $ LazyByteString.pack [0x61])
        f (Text.pack "\x100") `shouldBe` Nothing

    describe "TryFrom LazyText Latin1L" $ do
      let f = hush . Witch.tryFrom @LazyText.Text @Encoding.Latin1L
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Just (Tagged.Tagged $ LazyByteString.pack [0x61])
        f (LazyText.pack "\x100") `shouldBe` Nothing

    describe "TryFrom LazyText Latin1S" $ do
      let f = hush . Witch.tryFrom @LazyText.Text @Encoding.Latin1S
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Just (Tagged.Tagged $ ByteString.pack [0x61])
        f (LazyText.pack "\x100") `shouldBe` Nothing

    describe "TryFrom String Latin1S" $ do
      let f = hush . Witch.tryFrom @String @Encoding.Latin1S
      it "works" $ do
        f "a" `shouldBe` Just (Tagged.Tagged $ ByteString.pack [0x61])
        f "\x100" `shouldBe` Nothing

    describe "TryFrom String Latin1L" $ do
      let f = hush . Witch.tryFrom @String @Encoding.Latin1L
      it "works" $ do
        f "a" `shouldBe` Just (Tagged.Tagged $ LazyByteString.pack [0x61])
        f "\x100" `shouldBe` Nothing

    describe "TryFrom Utf8S Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf8S @Text.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [])) `shouldBe` Just (Text.pack "")
        f (Tagged.Tagged (ByteString.pack [0x61])) `shouldBe` Just (Text.pack "a")
        f (Tagged.Tagged (ByteString.pack [0xff])) `shouldBe` Nothing
        f (Tagged.Tagged (ByteString.pack [0x24])) `shouldBe` Just (Text.pack "\x24")
        f (Tagged.Tagged (ByteString.pack [0xc2, 0xa3])) `shouldBe` Just (Text.pack "\xa3")
        f (Tagged.Tagged (ByteString.pack [0xe2, 0x82, 0xac])) `shouldBe` Just (Text.pack "\x20ac")
        f (Tagged.Tagged (ByteString.pack [0xf0, 0x90, 0x8d, 0x88])) `shouldBe` Just (Text.pack "\x10348")

    describe "TryFrom Utf8S LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf8S @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [])) `shouldBe` Just (LazyText.pack "")
        f (Tagged.Tagged (ByteString.pack [0x61])) `shouldBe` Just (LazyText.pack "a")
        f (Tagged.Tagged (ByteString.pack [0xff])) `shouldBe` Nothing

    describe "TryFrom Utf8S String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf8S @String
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [])) `shouldBe` Just ""
        f (Tagged.Tagged (ByteString.pack [0x61])) `shouldBe` Just "a"
        f (Tagged.Tagged (ByteString.pack [0xff])) `shouldBe` Nothing

    describe "TryFrom Utf8L LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf8L @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [])) `shouldBe` Just (LazyText.pack "")
        f (Tagged.Tagged (LazyByteString.pack [0x61])) `shouldBe` Just (LazyText.pack "a")
        f (Tagged.Tagged (LazyByteString.pack [0xff])) `shouldBe` Nothing

    describe "TryFrom Utf8L Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf8L @Text.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [])) `shouldBe` Just (Text.pack "")
        f (Tagged.Tagged (LazyByteString.pack [0x61])) `shouldBe` Just (Text.pack "a")
        f (Tagged.Tagged (LazyByteString.pack [0xff])) `shouldBe` Nothing

    describe "TryFrom Utf8L String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf8L @String
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [])) `shouldBe` Just ""
        f (Tagged.Tagged (LazyByteString.pack [0x61])) `shouldBe` Just "a"
        f (Tagged.Tagged (LazyByteString.pack [0xff])) `shouldBe` Nothing

    describe "From Text Utf8S" $ do
      let f = Witch.from @Text.Text @Encoding.Utf8S
      it "works" $ do
        f (Text.pack "") `shouldBe` Tagged.Tagged (ByteString.pack [])
        f (Text.pack "a") `shouldBe` Tagged.Tagged (ByteString.pack [0x61])
        f (Text.pack "\x24") `shouldBe` Tagged.Tagged (ByteString.pack [0x24])
        f (Text.pack "\xa3") `shouldBe` Tagged.Tagged (ByteString.pack [0xc2, 0xa3])
        f (Text.pack "\x20ac") `shouldBe` Tagged.Tagged (ByteString.pack [0xe2, 0x82, 0xac])
        f (Text.pack "\x10348") `shouldBe` Tagged.Tagged (ByteString.pack [0xf0, 0x90, 0x8d, 0x88])

    describe "From Text Utf8L" $ do
      let f = Witch.from @Text.Text @Encoding.Utf8L
      it "works" $ do
        f (Text.pack "") `shouldBe` Tagged.Tagged (LazyByteString.pack [])
        f (Text.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61])

    describe "From LazyText Utf8L" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf8L
      it "works" $ do
        f (LazyText.pack "") `shouldBe` Tagged.Tagged (LazyByteString.pack [])
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61])

    describe "From LazyText Utf8S" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf8S
      it "works" $ do
        f (LazyText.pack "") `shouldBe` Tagged.Tagged (ByteString.pack [])
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (ByteString.pack [0x61])

    describe "From String Utf8S" $ do
      let f = Witch.from @String @Encoding.Utf8S
      it "works" $ do
        f "" `shouldBe` Tagged.Tagged (ByteString.pack [])
        f "a" `shouldBe` Tagged.Tagged (ByteString.pack [0x61])

    describe "From String Utf8L" $ do
      let f = Witch.from @String @Encoding.Utf8L
      it "works" $ do
        f "" `shouldBe` Tagged.Tagged (LazyByteString.pack [])
        f "a" `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61])

    describe "TryFrom Utf16LS Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16LS @Text.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [])) `shouldBe` Just (Text.pack "")
        f (Tagged.Tagged (ByteString.pack [0x24, 0x00])) `shouldBe` Just (Text.pack "\x24")
        f (Tagged.Tagged (ByteString.pack [0xa3, 0x00])) `shouldBe` Just (Text.pack "\xa3")
        f (Tagged.Tagged (ByteString.pack [0xac, 0x20])) `shouldBe` Just (Text.pack "\x20ac")
        f (Tagged.Tagged (ByteString.pack [0x00, 0xd8, 0x48, 0xdf])) `shouldBe` Just (Text.pack "\x10348")
        f (Tagged.Tagged (ByteString.pack [0x00])) `shouldBe` Nothing

    describe "TryFrom Utf16LS LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16LS @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x61, 0x00])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf16LS String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16LS @String
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x61, 0x00])) `shouldBe` Just "a"

    describe "TryFrom Utf16LL LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16LL @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61, 0x00])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf16LL Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16LL @Text.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61, 0x00])) `shouldBe` Just (Text.pack "a")

    describe "TryFrom Utf16LL String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16LL @String
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61, 0x00])) `shouldBe` Just "a"

    describe "From Text Utf16LS" $ do
      let f = Witch.from @Text.Text @Encoding.Utf16LS
      it "works" $ do
        f (Text.pack "") `shouldBe` Tagged.Tagged (ByteString.pack [])
        f (Text.pack "\x24") `shouldBe` Tagged.Tagged (ByteString.pack [0x24, 0x00])
        f (Text.pack "\xa3") `shouldBe` Tagged.Tagged (ByteString.pack [0xa3, 0x00])
        f (Text.pack "\x20ac") `shouldBe` Tagged.Tagged (ByteString.pack [0xac, 0x20])
        f (Text.pack "\x10348") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0xd8, 0x48, 0xdf])

    describe "From Text Utf16LL" $ do
      let f = Witch.from @Text.Text @Encoding.Utf16LL
      it "works" $ do
        f (Text.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61, 0x00])

    describe "From LazyText Utf16LL" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf16LL
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61, 0x00])

    describe "From LazyText Utf16LS" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf16LS
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (ByteString.pack [0x61, 0x00])

    describe "From String Utf16LS" $ do
      let f = Witch.from @String @Encoding.Utf16LS
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (ByteString.pack [0x61, 0x00])

    describe "From String Utf16LL" $ do
      let f = Witch.from @String @Encoding.Utf16LL
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61, 0x00])

    describe "TryFrom Utf16BS Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16BS @Text.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [])) `shouldBe` Just (Text.pack "")
        f (Tagged.Tagged (ByteString.pack [0x00, 0x24])) `shouldBe` Just (Text.pack "\x24")
        f (Tagged.Tagged (ByteString.pack [0x00, 0xa3])) `shouldBe` Just (Text.pack "\xa3")
        f (Tagged.Tagged (ByteString.pack [0x20, 0xac])) `shouldBe` Just (Text.pack "\x20ac")
        f (Tagged.Tagged (ByteString.pack [0xd8, 0x00, 0xdf, 0x48])) `shouldBe` Just (Text.pack "\x10348")
        f (Tagged.Tagged (ByteString.pack [0x00])) `shouldBe` Nothing

    describe "TryFrom Utf16BS LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16BS @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x00, 0x61])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf16BS String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16BS @String
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x00, 0x61])) `shouldBe` Just "a"

    describe "TryFrom Utf16BL LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16BL @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x00, 0x61])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf16BL Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16BL @Text.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x00, 0x61])) `shouldBe` Just (Text.pack "a")

    describe "TryFrom Utf16BL String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf16BL @String
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x00, 0x61])) `shouldBe` Just "a"

    describe "From Text Utf16BS" $ do
      let f = Witch.from @Text.Text @Encoding.Utf16BS
      it "works" $ do
        f (Text.pack "") `shouldBe` Tagged.Tagged (ByteString.pack [])
        f (Text.pack "\x24") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x24])
        f (Text.pack "\xa3") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0xa3])
        f (Text.pack "\x20ac") `shouldBe` Tagged.Tagged (ByteString.pack [0x20, 0xac])
        f (Text.pack "\x10348") `shouldBe` Tagged.Tagged (ByteString.pack [0xd8, 0x00, 0xdf, 0x48])

    describe "From Text Utf16BL" $ do
      let f = Witch.from @Text.Text @Encoding.Utf16BL
      it "works" $ do
        f (Text.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x00, 0x61])

    describe "From LazyText Utf16BL" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf16BL
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x00, 0x61])

    describe "From LazyText Utf16BS" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf16BS
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x61])

    describe "From String Utf16BS" $ do
      let f = Witch.from @String @Encoding.Utf16BS
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x61])

    describe "From String Utf16BL" $ do
      let f = Witch.from @String @Encoding.Utf16BL
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (LazyByteString.pack [0x00, 0x61])

    describe "TryFrom Utf32LS Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32LS @Text.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [])) `shouldBe` Just (Text.pack "")
        f (Tagged.Tagged (ByteString.pack [0x24, 0x00, 0x00, 0x00])) `shouldBe` Just (Text.pack "\x24")
        f (Tagged.Tagged (ByteString.pack [0xa3, 0x00, 0x00, 0x00])) `shouldBe` Just (Text.pack "\xa3")
        f (Tagged.Tagged (ByteString.pack [0xac, 0x20, 0x00, 0x00])) `shouldBe` Just (Text.pack "\x20ac")
        f (Tagged.Tagged (ByteString.pack [0x48, 0x03, 0x01, 0x00])) `shouldBe` Just (Text.pack "\x10348")
        f (Tagged.Tagged (ByteString.pack [0x00])) `shouldBe` Nothing

    describe "TryFrom Utf32LS LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32LS @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x61, 0x00, 0x00, 0x00])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf32LS String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32LS @String
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x61, 0x00, 0x00, 0x00])) `shouldBe` Just "a"

    describe "TryFrom Utf32LL LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32LL @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61, 0x00, 0x00, 0x00])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf32LL Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32LL @Text.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61, 0x00, 0x00, 0x00])) `shouldBe` Just (Text.pack "a")

    describe "TryFrom Utf32LL String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32LL @String
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x61, 0x00, 0x00, 0x00])) `shouldBe` Just "a"

    describe "From Text Utf32LS" $ do
      let f = Witch.from @Text.Text @Encoding.Utf32LS
      it "works" $ do
        f (Text.pack "") `shouldBe` Tagged.Tagged (ByteString.pack [])
        f (Text.pack "\x24") `shouldBe` Tagged.Tagged (ByteString.pack [0x24, 0x00, 0x00, 0x00])
        f (Text.pack "\xa3") `shouldBe` Tagged.Tagged (ByteString.pack [0xa3, 0x00, 0x00, 0x00])
        f (Text.pack "\x20ac") `shouldBe` Tagged.Tagged (ByteString.pack [0xac, 0x20, 0x00, 0x00])
        f (Text.pack "\x10348") `shouldBe` Tagged.Tagged (ByteString.pack [0x48, 0x03, 0x01, 0x00])

    describe "From Text Utf32LL" $ do
      let f = Witch.from @Text.Text @Encoding.Utf32LL
      it "works" $ do
        f (Text.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61, 0x00, 0x00, 0x00])

    describe "From LazyText Utf32LL" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf32LL
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61, 0x00, 0x00, 0x00])

    describe "From LazyText Utf32LS" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf32LS
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (ByteString.pack [0x61, 0x00, 0x00, 0x00])

    describe "From String Utf32LS" $ do
      let f = Witch.from @String @Encoding.Utf32LS
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (ByteString.pack [0x61, 0x00, 0x00, 0x00])

    describe "From String Utf32LL" $ do
      let f = Witch.from @String @Encoding.Utf32LL
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (LazyByteString.pack [0x61, 0x00, 0x00, 0x00])

    describe "TryFrom Utf32BS Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32BS @Text.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [])) `shouldBe` Just (Text.pack "")
        f (Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0x24])) `shouldBe` Just (Text.pack "\x24")
        f (Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0xa3])) `shouldBe` Just (Text.pack "\xa3")
        f (Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x20, 0xac])) `shouldBe` Just (Text.pack "\x20ac")
        f (Tagged.Tagged (ByteString.pack [0x00, 0x01, 0x03, 0x48])) `shouldBe` Just (Text.pack "\x10348")
        f (Tagged.Tagged (ByteString.pack [0x00])) `shouldBe` Nothing

    describe "TryFrom Utf32BS LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32BS @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0x61])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf32BS String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32BS @String
      it "works" $ do
        f (Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0x61])) `shouldBe` Just "a"

    describe "TryFrom Utf32BL LazyText" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32BL @LazyText.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x00, 0x00, 0x00, 0x61])) `shouldBe` Just (LazyText.pack "a")

    describe "TryFrom Utf32BL Text" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32BL @Text.Text
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x00, 0x00, 0x00, 0x61])) `shouldBe` Just (Text.pack "a")

    describe "TryFrom Utf32BL String" $ do
      let f = hush . Witch.tryFrom @Encoding.Utf32BL @String
      it "works" $ do
        f (Tagged.Tagged (LazyByteString.pack [0x00, 0x00, 0x00, 0x61])) `shouldBe` Just "a"

    describe "From Text Utf32BS" $ do
      let f = Witch.from @Text.Text @Encoding.Utf32BS
      it "works" $ do
        f (Text.pack "") `shouldBe` Tagged.Tagged (ByteString.pack [])
        f (Text.pack "\x24") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0x24])
        f (Text.pack "\xa3") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0xa3])
        f (Text.pack "\x20ac") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x20, 0xac])
        f (Text.pack "\x10348") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x01, 0x03, 0x48])

    describe "From Text Utf32BL" $ do
      let f = Witch.from @Text.Text @Encoding.Utf32BL
      it "works" $ do
        f (Text.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x00, 0x00, 0x00, 0x61])

    describe "From LazyText Utf32BL" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf32BL
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (LazyByteString.pack [0x00, 0x00, 0x00, 0x61])

    describe "From LazyText Utf32BS" $ do
      let f = Witch.from @LazyText.Text @Encoding.Utf32BS
      it "works" $ do
        f (LazyText.pack "a") `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0x61])

    describe "From String Utf32BS" $ do
      let f = Witch.from @String @Encoding.Utf32BS
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (ByteString.pack [0x00, 0x00, 0x00, 0x61])

    describe "From String Utf32BL" $ do
      let f = Witch.from @String @Encoding.Utf32BL
      it "works" $ do
        f "a" `shouldBe` Tagged.Tagged (LazyByteString.pack [0x00, 0x00, 0x00, 0x61])

  describe "Generically" $ do
    it "converts into empty" $ do
      -- This only needs to type check.
      let _ = Witch.from @Void.Void @Empty
      pure ()

    it "converts into unit" $ do
      let f = Witch.from @() @Unit
      f () `shouldBe` MkUnit

    it "converts into only" $ do
      let f = Witch.from @(Tuple.Solo Int) @(Only Integer)
      f (Tuple.MkSolo 1) `shouldBe` MkOnly 1

    it "converts into pair" $ do
      let f = Witch.from @(Int, Int.Int8) @(Pair Integer Age)
      f (1, 2) `shouldBe` MkPair 1 (MkAge 2)

    it "converts into result" $ do
      let f = Witch.from @(Either Int Int.Int8) @(Result Integer Age)
      f (Left 1) `shouldBe` Failure 1
      f (Right 2) `shouldBe` Success (MkAge 2)

    it "converts into list" $ do
      let f = Witch.from @[Int] @(List Integer)
      f [] `shouldBe` Nil
      f [1] `shouldBe` Cons 1 Nil
      f [1, 2] `shouldBe` Cons 1 (Cons 2 Nil)

data Empty
  deriving (Generics.Generic)

deriving via Generics.Generically Empty instance Witch.From Void.Void Empty

data Unit
  = MkUnit
  deriving (Eq, Generics.Generic, Show)

deriving via Generics.Generically Unit instance Witch.From () Unit

newtype Only a
  = MkOnly a
  deriving (Eq, Generics.Generic, Show)

deriving via
  Generics.Generically (Only b)
  instance
    (Witch.From a b) => Witch.From (Tuple.Solo a) (Only b)

data Pair a b
  = MkPair a b
  deriving (Eq, Generics.Generic, Show)

deriving via
  Generics.Generically (Pair c d)
  instance
    (Witch.From a c, Witch.From b d) => Witch.From (a, b) (Pair c d)

data Result a b
  = Failure a
  | Success b
  deriving (Eq, Generics.Generic, Show)

deriving via
  Generics.Generically (Result c d)
  instance
    (Witch.From a c, Witch.From b d) => Witch.From (Either a b) (Result c d)

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Generics.Generic, Show)

deriving via
  Generics.Generically (List b)
  instance
    (Witch.From a b) => Witch.From [a] (List b)

newtype Age
  = MkAge Int.Int8
  deriving (Eq, Show)

instance Witch.From Age Int.Int8

instance Witch.From Int.Int8 Age

type Selector e = e -> Bool

type Spec = Writer.Writer (Seq.Seq HUnit.Test) ()

anyTryFromException :: Selector (Witch.TryFromException s t)
anyTryFromException = const True

describe :: (Stack.HasCallStack) => String -> Spec -> Spec
describe label = testToSpec . HUnit.TestLabel label . specToTest

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

it :: (Stack.HasCallStack) => String -> HUnit.Assertion -> Spec
it label = testToSpec . HUnit.TestLabel label . HUnit.TestCase

shouldBe :: (Stack.HasCallStack, Eq a, Show a) => a -> a -> HUnit.Assertion
shouldBe = (HUnit.@?=)

shouldSatisfy :: (Stack.HasCallStack, Show a) => a -> (a -> Bool) -> HUnit.Assertion
shouldSatisfy value predicate = HUnit.assertBool ("predicate failed on: " <> show value) $ predicate value

shouldThrow :: (Stack.HasCallStack, Exception.Exception e) => IO a -> Selector e -> HUnit.Assertion
shouldThrow action predicate = do
  result <- Exception.try action
  case result of
    Right _ -> HUnit.assertFailure "did not get expected exception"
    Left exception -> HUnit.assertBool ("predicate failed on expected exception: " <> show exception) $ predicate exception

specToTest :: (Stack.HasCallStack) => Spec -> HUnit.Test
specToTest = HUnit.TestList . Foldable.toList . Writer.execWriter

testToSpec :: (Stack.HasCallStack) => HUnit.Test -> Spec
testToSpec = Writer.tell . Seq.singleton

unixEpoch :: Time.UTCTime
unixEpoch = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
