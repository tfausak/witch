{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
import Control.Monad
import Data.Complex
import Data.Either
import Data.Fixed
import Data.Int
import Data.List.NonEmpty
import Data.Ratio
import Data.Word
import Numeric.Natural
import Test.Hspec
import Witch

main :: IO ()
main = hspec . describe "Witch" $ do

  describe "Cast" $ do

    describe "cast" $ do
      test $ cast (1 :: Int8) `shouldBe` (1 :: Int16)

  describe "TryCast" $ do

    describe "tryCast" $ do
      test $ tryCast (1 :: Int16) `shouldBe` Right (1 :: Int8)
      test $ tryCast 128 `shouldBe` Left (TryCastException @Int16 @Int8 128)

  describe "Utility" $ do

    describe "as" $ do
      test $ as @Int8 1 `shouldBe` 1

    describe "from" $ do
      test $ from @Int8 1 `shouldBe` (1 :: Int16)

    describe "into" $ do
      test $ into @Int16 (1 :: Int8) `shouldBe` 1

    describe "over" $ do
      test $ over @String (<> "!") (Name "Kiki") `shouldBe` Name "Kiki!"

    describe "via" $ do
      test $ via @Int16 (1 :: Int8) `shouldBe` (1 :: Int32)

    describe "tryFrom" $ do
      test $ tryFrom @Int16 1 `shouldBe` Right (1 :: Int8)

    describe "tryInto" $ do
      test $ tryInto @Int8 (1 :: Int16) `shouldBe` Right 1

    describe "unsafeCast" $ do
      test $ unsafeCast (1 :: Int16) `shouldBe` (1 :: Int8)
      test $ evaluate (unsafeCast @Int16 @Int8 128) `shouldThrow` (== TryCastException @Int16 @Int8 128)

    describe "unsafeFrom" $ do
      test $ unsafeFrom @Int16 1 `shouldBe` (1 :: Int8)

    describe "unsafeInto" $ do
      test $ unsafeInto @Int8 (1 :: Int16) `shouldBe` 1

  describe "Lift" $ do

    describe "liftedCast" $ do
      test $ ($$(liftedCast (1 :: Int16)) :: Int8) `shouldBe` 1

    describe "liftedFrom" $ do
      test $ ($$(liftedFrom @Int16 1) :: Int8) `shouldBe` 1

    describe "liftedInto" $ do
      test $ ($$(liftedInto @Int8 (1 :: Int16))) `shouldBe` 1

  describe "Instances" $ do

    -- Int8

    describe "Cast Int8 Int16" $ do
      test $ cast @Int8 @Int16 0 `shouldBe` 0
      test $ cast @Int8 @Int16 127 `shouldBe` 127
      test $ cast @Int8 @Int16 (-128) `shouldBe` (-128)

    describe "Cast Int8 Int32" $ do
      test $ cast @Int8 @Int32 0 `shouldBe` 0
      test $ cast @Int8 @Int32 127 `shouldBe` 127
      test $ cast @Int8 @Int32 (-128) `shouldBe` (-128)

    describe "Cast Int8 Int64" $ do
      test $ cast @Int8 @Int64 0 `shouldBe` 0
      test $ cast @Int8 @Int64 127 `shouldBe` 127
      test $ cast @Int8 @Int64 (-128) `shouldBe` (-128)

    describe "Cast Int8 Int" $ do
      test $ cast @Int8 @Int 0 `shouldBe` 0
      test $ cast @Int8 @Int 127 `shouldBe` 127
      test $ cast @Int8 @Int (-128) `shouldBe` (-128)

    describe "Cast Int8 Integer" $ do
      test $ cast @Int8 @Integer 0 `shouldBe` 0
      test $ cast @Int8 @Integer 127 `shouldBe` 127
      test $ cast @Int8 @Integer (-128) `shouldBe` (-128)

    describe "TryCast Int8 Word8" $ do
      test $ tryCast @Int8 @Word8 0 `shouldBe` Right 0
      test $ tryCast @Int8 @Word8 127 `shouldBe` Right 127
      test $ tryCast @Int8 @Word8 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int8 Word16" $ do
      test $ tryCast @Int8 @Word16 0 `shouldBe` Right 0
      test $ tryCast @Int8 @Word16 127 `shouldBe` Right 127
      test $ tryCast @Int8 @Word16 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int8 Word32" $ do
      test $ tryCast @Int8 @Word32 0 `shouldBe` Right 0
      test $ tryCast @Int8 @Word32 127 `shouldBe` Right 127
      test $ tryCast @Int8 @Word32 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int8 Word64" $ do
      test $ tryCast @Int8 @Word64 0 `shouldBe` Right 0
      test $ tryCast @Int8 @Word64 127 `shouldBe` Right 127
      test $ tryCast @Int8 @Word64 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int8 Word" $ do
      test $ tryCast @Int8 @Word 0 `shouldBe` Right 0
      test $ tryCast @Int8 @Word 127 `shouldBe` Right 127
      test $ tryCast @Int8 @Word (-1) `shouldSatisfy` isLeft

    describe "TryCast Int8 Natural" $ do
      test $ tryCast @Int8 @Natural 0 `shouldBe` Right 0
      test $ tryCast @Int8 @Natural 127 `shouldBe` Right 127
      test $ tryCast @Int8 @Natural (-1) `shouldSatisfy` isLeft

    describe "Cast Int8 Float" $ do
      test $ cast @Int8 @Float 0 `shouldBe` 0
      test $ cast @Int8 @Float 127 `shouldBe` 127
      test $ cast @Int8 @Float (-128) `shouldBe` (-128)

    describe "Cast Int8 Double" $ do
      test $ cast @Int8 @Double 0 `shouldBe` 0
      test $ cast @Int8 @Double 127 `shouldBe` 127
      test $ cast @Int8 @Double (-128) `shouldBe` (-128)

    -- Int16

    describe "TryCast Int16 Int8" $ do
      test $ tryCast @Int16 @Int8 0 `shouldBe` Right 0
      test $ tryCast @Int16 @Int8 127 `shouldBe` Right 127
      test $ tryCast @Int16 @Int8 128 `shouldSatisfy` isLeft
      test $ tryCast @Int16 @Int8 (-128) `shouldBe` Right (-128)
      test $ tryCast @Int16 @Int8 (-129) `shouldSatisfy` isLeft

    describe "Cast Int16 Int32" $ do
      test $ cast @Int16 @Int32 0 `shouldBe` 0
      test $ cast @Int16 @Int32 32767 `shouldBe` 32767
      test $ cast @Int16 @Int32 (-32768) `shouldBe` (-32768)

    describe "Cast Int16 Int64" $ do
      test $ cast @Int16 @Int64 0 `shouldBe` 0
      test $ cast @Int16 @Int64 32767 `shouldBe` 32767
      test $ cast @Int16 @Int64 (-32768) `shouldBe` (-32768)

    describe "Cast Int16 Int" $ do
      test $ cast @Int16 @Int 0 `shouldBe` 0
      test $ cast @Int16 @Int 32767 `shouldBe` 32767
      test $ cast @Int16 @Int (-32768) `shouldBe` (-32768)

    describe "Cast Int16 Integer" $ do
      test $ cast @Int16 @Integer 0 `shouldBe` 0
      test $ cast @Int16 @Integer 32767 `shouldBe` 32767
      test $ cast @Int16 @Integer (-32768) `shouldBe` (-32768)

    describe "TryCast Int16 Word8" $ do
      test $ tryCast @Int16 @Word8 0 `shouldBe` Right 0
      test $ tryCast @Int16 @Word8 255 `shouldBe` Right 255
      test $ tryCast @Int16 @Word8 256 `shouldSatisfy` isLeft
      test $ tryCast @Int16 @Word8 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int16 Word16" $ do
      test $ tryCast @Int16 @Word16 0 `shouldBe` Right 0
      test $ tryCast @Int16 @Word16 127 `shouldBe` Right 127
      test $ tryCast @Int16 @Word16 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int16 Word32" $ do
      test $ tryCast @Int16 @Word32 0 `shouldBe` Right 0
      test $ tryCast @Int16 @Word32 32767 `shouldBe` Right 32767
      test $ tryCast @Int16 @Word32 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int16 Word64" $ do
      test $ tryCast @Int16 @Word64 0 `shouldBe` Right 0
      test $ tryCast @Int16 @Word64 32767 `shouldBe` Right 32767
      test $ tryCast @Int16 @Word64 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int16 Word" $ do
      test $ tryCast @Int16 @Word 0 `shouldBe` Right 0
      test $ tryCast @Int16 @Word 32767 `shouldBe` Right 32767
      test $ tryCast @Int16 @Word (-1) `shouldSatisfy` isLeft

    describe "TryCast Int16 Natural" $ do
      test $ tryCast @Int16 @Natural 0 `shouldBe` Right 0
      test $ tryCast @Int16 @Natural 32767 `shouldBe` Right 32767
      test $ tryCast @Int16 @Natural (-1) `shouldSatisfy` isLeft

    describe "Cast Int16 Float" $ do
      test $ cast @Int16 @Float 0 `shouldBe` 0
      test $ cast @Int16 @Float 32767 `shouldBe` 32767
      test $ cast @Int16 @Float (-32768) `shouldBe` (-32768)

    describe "Cast Int16 Double" $ do
      test $ cast @Int16 @Double 0 `shouldBe` 0
      test $ cast @Int16 @Double 32767 `shouldBe` 32767
      test $ cast @Int16 @Double (-32768) `shouldBe` (-32768)

    -- Int32

    describe "TryCast Int32 Int8" $ do
      test $ tryCast @Int32 @Int8 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Int8 127 `shouldBe` Right 127
      test $ tryCast @Int32 @Int8 128 `shouldSatisfy` isLeft
      test $ tryCast @Int32 @Int8 (-128) `shouldBe` Right (-128)
      test $ tryCast @Int32 @Int8 (-129) `shouldSatisfy` isLeft

    describe "TryCast Int32 Int16" $ do
      test $ tryCast @Int32 @Int16 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Int32 @Int16 32768 `shouldSatisfy` isLeft
      test $ tryCast @Int32 @Int16 (-32768) `shouldBe` Right (-32768)
      test $ tryCast @Int32 @Int16 (-32769) `shouldSatisfy` isLeft

    describe "Cast Int32 Int64" $ do
      test $ cast @Int32 @Int64 0 `shouldBe` 0
      test $ cast @Int32 @Int64 2147483647 `shouldBe` 2147483647
      test $ cast @Int32 @Int64 (-2147483648) `shouldBe` (-2147483648)

    describe "TryCast Int32 Int" $ do
      when (toInteger (maxBound :: Int) < 2147483647) untested
      test $ tryCast @Int32 @Int 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Int 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int32 @Int (-2147483648) `shouldBe` Right (-2147483648)

    describe "Cast Int32 Integer" $ do
      test $ cast @Int32 @Integer 0 `shouldBe` 0
      test $ cast @Int32 @Integer 2147483647 `shouldBe` 2147483647
      test $ cast @Int32 @Integer (-2147483648) `shouldBe` (-2147483648)

    describe "TryCast Int32 Word8" $ do
      test $ tryCast @Int32 @Word8 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Word8 255 `shouldBe` Right 255
      test $ tryCast @Int32 @Word8 256 `shouldSatisfy` isLeft
      test $ tryCast @Int32 @Word8 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int32 Word16" $ do
      test $ tryCast @Int32 @Word16 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Int32 @Word16 65536 `shouldSatisfy` isLeft
      test $ tryCast @Int32 @Word16 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int32 Word32" $ do
      test $ tryCast @Int32 @Word32 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Word32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int32 @Word32 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int32 Word64" $ do
      test $ tryCast @Int32 @Word64 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Word64 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int32 @Word64 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int32 Word" $ do
      when (toInteger (maxBound :: Word) < 2147483647) untested
      test $ tryCast @Int32 @Word 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Word 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int32 @Word (-1) `shouldSatisfy` isLeft

    describe "TryCast Int32 Natural" $ do
      test $ tryCast @Int32 @Natural 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Natural 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int32 @Natural (-1) `shouldSatisfy` isLeft

    describe "TryCast Int32 Float" $ do
      test $ tryCast @Int32 @Float 0 `shouldBe` Right 0
      test $ tryCast @Int32 @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Int32 @Float 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Int32 @Float (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Int32 @Float (-16777216) `shouldSatisfy` isLeft

    describe "Cast Int32 Double" $ do
      test $ cast @Int32 @Double 0 `shouldBe` 0
      test $ cast @Int32 @Double 2147483647 `shouldBe` 2147483647
      test $ cast @Int32 @Double (-2147483648) `shouldBe` (-2147483648)

    -- Int64

    describe "TryCast Int64 Int8" $ do
      test $ tryCast @Int64 @Int8 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Int8 127 `shouldBe` Right 127
      test $ tryCast @Int64 @Int8 128 `shouldSatisfy` isLeft
      test $ tryCast @Int64 @Int8 (-128) `shouldBe` Right (-128)
      test $ tryCast @Int64 @Int8 (-129) `shouldSatisfy` isLeft

    describe "TryCast Int64 Int16" $ do
      test $ tryCast @Int64 @Int16 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Int64 @Int16 32768 `shouldSatisfy` isLeft
      test $ tryCast @Int64 @Int16 (-32768) `shouldBe` Right (-32768)
      test $ tryCast @Int64 @Int16 (-32769) `shouldSatisfy` isLeft

    describe "TryCast Int64 Int32" $ do
      test $ tryCast @Int64 @Int32 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Int32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int64 @Int32 2147483648 `shouldSatisfy` isLeft
      test $ tryCast @Int64 @Int32 (-2147483648) `shouldBe` Right (-2147483648)
      test $ tryCast @Int64 @Int32 (-2147483649) `shouldSatisfy` isLeft

    describe "TryCast Int64 Int" $ do
      when (toInteger (maxBound :: Int) < 9223372036854775807) untested
      test $ tryCast @Int64 @Int 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Int 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Int64 @Int (-9223372036854775808) `shouldBe` Right (-9223372036854775808)

    describe "Cast Int64 Integer" $ do
      test $ cast @Int64 @Integer 0 `shouldBe` 0
      test $ cast @Int64 @Integer 9223372036854775807 `shouldBe` 9223372036854775807
      test $ cast @Int64 @Integer (-9223372036854775808) `shouldBe` (-9223372036854775808)

    describe "TryCast Int64 Word8" $ do
      test $ tryCast @Int64 @Word8 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Word8 255 `shouldBe` Right 255
      test $ tryCast @Int64 @Word8 256 `shouldSatisfy` isLeft
      test $ tryCast @Int64 @Word8 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int64 Word16" $ do
      test $ tryCast @Int64 @Word16 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Int64 @Word16 65536 `shouldSatisfy` isLeft
      test $ tryCast @Int64 @Word16 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int64 Word32" $ do
      test $ tryCast @Int64 @Word32 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Word32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int64 @Word32 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int64 Word64" $ do
      test $ tryCast @Int64 @Word64 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Word64 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Int64 @Word64 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int64 Word" $ do
      when (toInteger (maxBound :: Word) < 9223372036854775807) untested
      test $ tryCast @Int64 @Word 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Word 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Int64 @Word (-1) `shouldSatisfy` isLeft

    describe "TryCast Int64 Natural" $ do
      test $ tryCast @Int64 @Natural 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Natural 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Int64 @Natural (-1) `shouldSatisfy` isLeft

    describe "TryCast Int64 Float" $ do
      test $ tryCast @Int64 @Float 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Int64 @Float 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Int64 @Float (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Int64 @Float (-16777216) `shouldSatisfy` isLeft

    describe "TryCast Int64 Double" $ do
      test $ tryCast @Int64 @Double 0 `shouldBe` Right 0
      test $ tryCast @Int64 @Double 9007199254740991 `shouldBe` Right 9007199254740991
      test $ tryCast @Int64 @Double 9007199254740992 `shouldSatisfy` isLeft
      test $ tryCast @Int64 @Double (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ tryCast @Int64 @Double (-9007199254740992) `shouldSatisfy` isLeft

    -- Int

    describe "TryCast Int Int8" $ do
      test $ tryCast @Int @Int8 0 `shouldBe` Right 0
      test $ tryCast @Int @Int8 127 `shouldBe` Right 127
      test $ tryCast @Int @Int8 128 `shouldSatisfy` isLeft
      test $ tryCast @Int @Int8 (-128) `shouldBe` Right (-128)
      test $ tryCast @Int @Int8 (-129) `shouldSatisfy` isLeft

    describe "TryCast Int Int16" $ do
      test $ tryCast @Int @Int16 0 `shouldBe` Right 0
      test $ tryCast @Int @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Int @Int16 32768 `shouldSatisfy` isLeft
      test $ tryCast @Int @Int16 (-32768) `shouldBe` Right (-32768)
      test $ tryCast @Int @Int16 (-32769) `shouldSatisfy` isLeft

    describe "TryCast Int Int32" $ do
      when (toInteger (maxBound :: Int) < 2147483647) untested
      test $ tryCast @Int @Int32 0 `shouldBe` Right 0
      test $ tryCast @Int @Int32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Int @Int32 2147483648 `shouldSatisfy` isLeft
      test $ tryCast @Int @Int32 (-2147483648) `shouldBe` Right (-2147483648)
      test $ tryCast @Int @Int32 (-2147483649) `shouldSatisfy` isLeft

    describe "Cast Int Int64" $ do
      test $ cast @Int @Int64 0 `shouldBe` 0
      test $ cast @Int @Int64 maxBound `shouldBe` fromIntegral (maxBound :: Int)
      test $ cast @Int @Int64 minBound `shouldBe` fromIntegral (minBound :: Int)

    describe "Cast Int Integer" $ do
      test $ cast @Int @Integer 0 `shouldBe` 0
      test $ cast @Int @Integer maxBound `shouldBe` fromIntegral (maxBound :: Int)
      test $ cast @Int @Integer minBound `shouldBe` fromIntegral (minBound :: Int)

    describe "TryCast Int Word8" $ do
      test $ tryCast @Int @Word8 0 `shouldBe` Right 0
      test $ tryCast @Int @Word8 255 `shouldBe` Right 255
      test $ tryCast @Int @Word8 256 `shouldSatisfy` isLeft
      test $ tryCast @Int @Word8 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int Word16" $ do
      test $ tryCast @Int @Word16 0 `shouldBe` Right 0
      test $ tryCast @Int @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Int @Word16 65536 `shouldSatisfy` isLeft
      test $ tryCast @Int @Word16 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int Word32" $ do
      when (toInteger (maxBound :: Int) < 4294967295) untested
      test $ tryCast @Int @Word32 0 `shouldBe` Right 0
      test $ tryCast @Int @Word32 4294967295 `shouldBe` Right 4294967295
      test $ tryCast @Int @Word32 4294967296 `shouldSatisfy` isLeft
      test $ tryCast @Int @Word32 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int Word64" $ do
      test $ tryCast @Int @Word64 0 `shouldBe` Right 0
      test $ tryCast @Int @Word64 maxBound `shouldBe` Right (fromIntegral (maxBound :: Int))
      test $ tryCast @Int @Word64 (-1) `shouldSatisfy` isLeft

    describe "TryCast Int Word" $ do
      test $ tryCast @Int @Word 0 `shouldBe` Right 0
      test $ tryCast @Int @Word maxBound `shouldBe` Right (fromIntegral (maxBound :: Int))
      test $ tryCast @Int @Word (-1) `shouldSatisfy` isLeft

    describe "TryCast Int Natural" $ do
      test $ tryCast @Int @Natural 0 `shouldBe` Right 0
      test $ tryCast @Int @Natural maxBound `shouldBe` Right (fromIntegral (maxBound :: Int))
      test $ tryCast @Int @Natural (-1) `shouldSatisfy` isLeft

    describe "TryCast Int Float" $ do
      test $ tryCast @Int @Float 0 `shouldBe` Right 0
      test $ tryCast @Int @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Int @Float 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Int @Float (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Int @Float (-16777216) `shouldSatisfy` isLeft

    describe "TryCast Int Double" $ do
      when (toInteger (maxBound :: Int) <= 9007199254740991) untested
      test $ tryCast @Int @Double 0 `shouldBe` Right 0
      test $ tryCast @Int @Double 9007199254740991 `shouldBe` Right 9007199254740991
      test $ tryCast @Int @Double 9007199254740992 `shouldSatisfy` isLeft
      test $ tryCast @Int @Double (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ tryCast @Int @Double (-9007199254740992) `shouldSatisfy` isLeft

    -- Integer

    describe "TryCast Integer Int8" $ do
      test $ tryCast @Integer @Int8 0 `shouldBe` Right 0
      test $ tryCast @Integer @Int8 127 `shouldBe` Right 127
      test $ tryCast @Integer @Int8 128 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Int8 (-128) `shouldBe` Right (-128)
      test $ tryCast @Integer @Int8 (-129) `shouldSatisfy` isLeft

    describe "TryCast Integer Int16" $ do
      test $ tryCast @Integer @Int16 0 `shouldBe` Right 0
      test $ tryCast @Integer @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Integer @Int16 32768 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Int16 (-32768) `shouldBe` Right (-32768)
      test $ tryCast @Integer @Int16 (-32769) `shouldSatisfy` isLeft

    describe "TryCast Integer Int32" $ do
      test $ tryCast @Integer @Int32 0 `shouldBe` Right 0
      test $ tryCast @Integer @Int32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Integer @Int32 2147483648 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Int32 (-2147483648) `shouldBe` Right (-2147483648)
      test $ tryCast @Integer @Int32 (-2147483649) `shouldSatisfy` isLeft

    describe "TryCast Integer Int64" $ do
      test $ tryCast @Integer @Int64 0 `shouldBe` Right 0
      test $ tryCast @Integer @Int64 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Integer @Int64 9223372036854775808 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Int64 (-9223372036854775808) `shouldBe` Right (-9223372036854775808)
      test $ tryCast @Integer @Int64 (-9223372036854775809) `shouldSatisfy` isLeft

    describe "TryCast Integer Int" $ do
      test $ tryCast @Integer @Int 0 `shouldBe` Right 0
      test $ do
        let x = maxBound :: Int
        tryCast @Integer @Int (fromIntegral x) `shouldBe` Right x
      test $ do
        let x = toInteger (maxBound :: Int) + 1
        tryCast @Integer @Int x `shouldSatisfy` isLeft
      test $ do
        let x = minBound :: Int
        tryCast @Integer @Int (fromIntegral x) `shouldBe` Right x
      test $ do
        let x = toInteger (minBound :: Int) - 1
        tryCast @Integer @Int x `shouldSatisfy` isLeft

    describe "TryCast Integer Word8" $ do
      test $ tryCast @Integer @Word8 0 `shouldBe` Right 0
      test $ tryCast @Integer @Word8 255 `shouldBe` Right 255
      test $ tryCast @Integer @Word8 256 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Word8 (-1) `shouldSatisfy` isLeft

    describe "TryCast Integer Word16" $ do
      test $ tryCast @Integer @Word16 0 `shouldBe` Right 0
      test $ tryCast @Integer @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Integer @Word16 65536 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Word16 (-1) `shouldSatisfy` isLeft

    describe "TryCast Integer Word32" $ do
      test $ tryCast @Integer @Word32 0 `shouldBe` Right 0
      test $ tryCast @Integer @Word32 4294967295 `shouldBe` Right 4294967295
      test $ tryCast @Integer @Word32 4294967296 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Word32 (-1) `shouldSatisfy` isLeft

    describe "TryCast Integer Word64" $ do
      test $ tryCast @Integer @Word64 0 `shouldBe` Right 0
      test $ tryCast @Integer @Word64 18446744073709551615 `shouldBe` Right 18446744073709551615
      test $ tryCast @Integer @Word64 18446744073709551616 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Word64 (-1) `shouldSatisfy` isLeft

    describe "TryCast Integer Word" $ do
      test $ tryCast @Integer @Word 0 `shouldBe` Right 0
      test $ do
        let x = maxBound :: Word
        tryCast @Integer @Word (fromIntegral x) `shouldBe` Right x
      test $ do
        let x = toInteger (maxBound :: Word) + 1
        tryCast @Integer @Word x `shouldSatisfy` isLeft
      test $ tryCast @Integer @Word (-1) `shouldSatisfy` isLeft

    describe "TryCast Integer Natural" $ do
      test $ tryCast @Integer @Natural 0 `shouldBe` Right 0
      test $ tryCast @Integer @Natural 18446744073709551616 `shouldBe` Right 18446744073709551616
      test $ tryCast @Integer @Natural (-1) `shouldSatisfy` isLeft

    describe "TryCast Integer Float" $ do
      test $ tryCast @Integer @Float 0 `shouldBe` Right 0
      test $ tryCast @Integer @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Integer @Float 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Float (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Integer @Float (-16777216) `shouldSatisfy` isLeft

    describe "TryCast Integer Double" $ do
      test $ tryCast @Integer @Double 0 `shouldBe` Right 0
      test $ tryCast @Integer @Double 9007199254740991 `shouldBe` Right 9007199254740991
      test $ tryCast @Integer @Double 9007199254740992 `shouldSatisfy` isLeft
      test $ tryCast @Integer @Double (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ tryCast @Integer @Double (-9007199254740992) `shouldSatisfy` isLeft

    -- Word8

    describe "Cast Word8 Word16" $ do
      test $ cast @Word8 @Word16 0 `shouldBe` 0
      test $ cast @Word8 @Word16 255 `shouldBe` 255

    describe "Cast Word8 Word32" $ do
      test $ cast @Word8 @Word32 0 `shouldBe` 0
      test $ cast @Word8 @Word32 255 `shouldBe` 255

    describe "Cast Word8 Word64" $ do
      test $ cast @Word8 @Word64 0 `shouldBe` 0
      test $ cast @Word8 @Word64 255 `shouldBe` 255

    describe "Cast Word8 Word" $ do
      test $ cast @Word8 @Word 0 `shouldBe` 0
      test $ cast @Word8 @Word 255 `shouldBe` 255

    describe "Cast Word8 Natural" $ do
      test $ cast @Word8 @Natural 0 `shouldBe` 0
      test $ cast @Word8 @Natural 255 `shouldBe` 255

    describe "TryCast Word8 Int8" $ do
      test $ tryCast @Word8 @Int8 0 `shouldBe` Right 0
      test $ tryCast @Word8 @Int8 127 `shouldBe` Right 127
      test $ tryCast @Word8 @Int8 128 `shouldSatisfy` isLeft

    describe "Cast Word8 Int16" $ do
      test $ cast @Word8 @Int16 0 `shouldBe` 0
      test $ cast @Word8 @Int16 255 `shouldBe` 255

    describe "Cast Word8 Int32" $ do
      test $ cast @Word8 @Int32 0 `shouldBe` 0
      test $ cast @Word8 @Int32 255 `shouldBe` 255

    describe "Cast Word8 Int64" $ do
      test $ cast @Word8 @Int64 0 `shouldBe` 0
      test $ cast @Word8 @Int64 255 `shouldBe` 255

    describe "Cast Word8 Int" $ do
      test $ cast @Word8 @Int 0 `shouldBe` 0
      test $ cast @Word8 @Int 255 `shouldBe` 255

    describe "Cast Word8 Integer" $ do
      test $ cast @Word8 @Integer 0 `shouldBe` 0
      test $ cast @Word8 @Integer 255 `shouldBe` 255

    describe "Cast Word8 Float" $ do
      test $ cast @Word8 @Float 0 `shouldBe` 0
      test $ cast @Word8 @Float 255 `shouldBe` 255

    describe "Cast Word8 Double" $ do
      test $ cast @Word8 @Double 0 `shouldBe` 0
      test $ cast @Word8 @Double 255 `shouldBe` 255

    -- Word16

    describe "TryCast Word16 Word8" $ do
      test $ tryCast @Word16 @Word8 0 `shouldBe` Right 0
      test $ tryCast @Word16 @Word8 255 `shouldBe` Right 255
      test $ tryCast @Word16 @Word8 256 `shouldSatisfy` isLeft

    describe "Cast Word16 Word32" $ do
      test $ cast @Word16 @Word32 0 `shouldBe` 0
      test $ cast @Word16 @Word32 65535 `shouldBe` 65535

    describe "Cast Word16 Word64" $ do
      test $ cast @Word16 @Word64 0 `shouldBe` 0
      test $ cast @Word16 @Word64 65535 `shouldBe` 65535

    describe "Cast Word16 Word" $ do
      test $ cast @Word16 @Word 0 `shouldBe` 0
      test $ cast @Word16 @Word 65535 `shouldBe` 65535

    describe "Cast Word16 Natural" $ do
      test $ cast @Word16 @Natural 0 `shouldBe` 0
      test $ cast @Word16 @Natural 65535 `shouldBe` 65535

    describe "TryCast Word16 Int8" $ do
      test $ tryCast @Word16 @Int8 0 `shouldBe` Right 0
      test $ tryCast @Word16 @Int8 127 `shouldBe` Right 127
      test $ tryCast @Word16 @Int8 128 `shouldSatisfy` isLeft

    describe "TryCast Word16 Int16" $ do
      test $ tryCast @Word16 @Int16 0 `shouldBe` Right 0
      test $ tryCast @Word16 @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Word16 @Int16 32768 `shouldSatisfy` isLeft

    describe "Cast Word16 Int32" $ do
      test $ cast @Word16 @Int32 0 `shouldBe` 0
      test $ cast @Word16 @Int32 65535 `shouldBe` 65535

    describe "Cast Word16 Int64" $ do
      test $ cast @Word16 @Int64 0 `shouldBe` 0
      test $ cast @Word16 @Int64 65535 `shouldBe` 65535

    describe "Cast Word16 Int" $ do
      test $ cast @Word16 @Int 0 `shouldBe` 0
      test $ cast @Word16 @Int 65535 `shouldBe` 65535

    describe "Cast Word16 Integer" $ do
      test $ cast @Word16 @Integer 0 `shouldBe` 0
      test $ cast @Word16 @Integer 65535 `shouldBe` 65535

    describe "Cast Word16 Float" $ do
      test $ cast @Word16 @Float 0 `shouldBe` 0
      test $ cast @Word16 @Float 65535 `shouldBe` 65535

    describe "Cast Word16 Double" $ do
      test $ cast @Word16 @Double 0 `shouldBe` 0
      test $ cast @Word16 @Double 65535 `shouldBe` 65535

    -- Word32

    describe "TryCast Word32 Word8" $ do
      test $ tryCast @Word32 @Word8 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Word8 255 `shouldBe` Right 255
      test $ tryCast @Word32 @Word8 256 `shouldSatisfy` isLeft

    describe "TryCast Word32 Word16" $ do
      test $ tryCast @Word32 @Word16 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Word32 @Word16 65536 `shouldSatisfy` isLeft

    describe "Cast Word32 Word64" $ do
      test $ cast @Word32 @Word64 0 `shouldBe` 0
      test $ cast @Word32 @Word64 4294967295 `shouldBe` 4294967295

    describe "TryCast Word32 Word" $ do
      when (toInteger (maxBound :: Word) < 4294967295) untested
      test $ tryCast @Word32 @Word 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Word 4294967295 `shouldBe` Right 4294967295

    describe "Cast Word32 Natural" $ do
      test $ cast @Word32 @Natural 0 `shouldBe` 0
      test $ cast @Word32 @Natural 4294967295 `shouldBe` 4294967295

    describe "TryCast Word32 Int8" $ do
      test $ tryCast @Word32 @Int8 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Int8 127 `shouldBe` Right 127
      test $ tryCast @Word32 @Int8 128 `shouldSatisfy` isLeft

    describe "TryCast Word32 Int16" $ do
      test $ tryCast @Word32 @Int16 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Word32 @Int16 32768 `shouldSatisfy` isLeft

    describe "TryCast Word32 Int32" $ do
      test $ tryCast @Word32 @Int32 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Int32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Word32 @Int32 2147483648 `shouldSatisfy` isLeft

    describe "Cast Word32 Int64" $ do
      test $ cast @Word32 @Int64 0 `shouldBe` 0
      test $ cast @Word32 @Int64 4294967295 `shouldBe` 4294967295

    describe "TryCast Word32 Int" $ do
      when (toInteger (maxBound :: Int) < 4294967295) untested
      test $ tryCast @Word32 @Int 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Int 4294967295 `shouldBe` Right 4294967295

    describe "Cast Word32 Integer" $ do
      test $ cast @Word32 @Integer 0 `shouldBe` 0
      test $ cast @Word32 @Integer 4294967295 `shouldBe` 4294967295

    describe "TryCast Word32 Float" $ do
      test $ tryCast @Word32 @Float 0 `shouldBe` Right 0
      test $ tryCast @Word32 @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Word32 @Float 16777216 `shouldSatisfy` isLeft

    describe "Cast Word32 Double" $ do
      test $ cast @Word32 @Double 0 `shouldBe` 0
      test $ cast @Word32 @Double 4294967295 `shouldBe` 4294967295

    -- Word64

    describe "TryCast Word64 Word8" $ do
      test $ tryCast @Word64 @Word8 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Word8 255 `shouldBe` Right 255
      test $ tryCast @Word64 @Word8 256 `shouldSatisfy` isLeft

    describe "TryCast Word64 Word16" $ do
      test $ tryCast @Word64 @Word16 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Word64 @Word16 65536 `shouldSatisfy` isLeft

    describe "TryCast Word64 Word32" $ do
      test $ tryCast @Word64 @Word32 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Word32 4294967295 `shouldBe` Right 4294967295
      test $ tryCast @Word64 @Word32 4294967296 `shouldSatisfy` isLeft

    describe "TryCast Word64 Word" $ do
      when (toInteger (maxBound :: Word) < 18446744073709551615) untested
      test $ tryCast @Word64 @Word 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Word 18446744073709551615 `shouldBe` Right 18446744073709551615

    describe "Cast Word64 Natural" $ do
      test $ cast @Word64 @Natural 0 `shouldBe` 0
      test $ cast @Word64 @Natural 18446744073709551615 `shouldBe` 18446744073709551615

    describe "TryCast Word64 Int8" $ do
      test $ tryCast @Word64 @Int8 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Int8 127 `shouldBe` Right 127
      test $ tryCast @Word64 @Int8 128 `shouldSatisfy` isLeft

    describe "TryCast Word64 Int16" $ do
      test $ tryCast @Word64 @Int16 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Word64 @Int16 32768 `shouldSatisfy` isLeft

    describe "TryCast Word64 Int32" $ do
      test $ tryCast @Word64 @Int32 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Int32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Word64 @Int32 2147483648 `shouldSatisfy` isLeft

    describe "TryCast Word64 Int64" $ do
      test $ tryCast @Word64 @Int64 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Int64 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Word64 @Int32 9223372036854775808 `shouldSatisfy` isLeft

    describe "TryCast Word64 Int" $ do
      test $ tryCast @Word64 @Int 0 `shouldBe` Right 0
      test $ do
        let x = maxBound :: Int
        tryCast @Word64 @Int (fromIntegral x) `shouldBe` Right x
      test $ do
        let x = fromIntegral (maxBound :: Int) + 1 :: Word64
        tryCast @Word64 @Int x `shouldSatisfy` isLeft

    describe "Cast Word64 Integer" $ do
      test $ cast @Word64 @Integer 0 `shouldBe` 0
      test $ cast @Word64 @Integer 18446744073709551615 `shouldBe` 18446744073709551615

    describe "TryCast Word64 Float" $ do
      test $ tryCast @Word64 @Float 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Word64 @Float 16777216 `shouldSatisfy` isLeft

    describe "TryCast Word64 Double" $ do
      test $ tryCast @Word64 @Double 0 `shouldBe` Right 0
      test $ tryCast @Word64 @Double 9007199254740991 `shouldBe` Right 9007199254740991
      test $ tryCast @Word64 @Double 9007199254740992 `shouldSatisfy` isLeft

    -- Word

    describe "TryCast Word Word8" $ do
      test $ tryCast @Word @Word8 0 `shouldBe` Right 0
      test $ tryCast @Word @Word8 255 `shouldBe` Right 255
      test $ tryCast @Word @Word8 256 `shouldSatisfy` isLeft

    describe "TryCast Word Word16" $ do
      test $ tryCast @Word @Word16 0 `shouldBe` Right 0
      test $ tryCast @Word @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Word @Word16 65536 `shouldSatisfy` isLeft

    describe "TryCast Word Word32" $ do
      when (toInteger (maxBound :: Word) < 4294967295) untested
      test $ tryCast @Word @Word32 0 `shouldBe` Right 0
      test $ tryCast @Word @Word32 4294967295 `shouldBe` Right 4294967295
      test $ tryCast @Word @Word32 4294967296 `shouldSatisfy` isLeft

    describe "Cast Word Word64" $ do
      test $ cast @Word @Word64 0 `shouldBe` 0
      test $ cast @Word @Word64 maxBound `shouldBe` fromIntegral (maxBound :: Word)

    describe "Cast Word Natural" $ do
      test $ cast @Word @Natural 0 `shouldBe` 0
      test $ cast @Word @Natural maxBound `shouldBe` fromIntegral (maxBound :: Word)

    describe "TryCast Word Int8" $ do
      test $ tryCast @Word @Int8 0 `shouldBe` Right 0
      test $ tryCast @Word @Int8 127 `shouldBe` Right 127
      test $ tryCast @Word @Int8 128 `shouldSatisfy` isLeft

    describe "TryCast Word Int16" $ do
      test $ tryCast @Word @Int16 0 `shouldBe` Right 0
      test $ tryCast @Word @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Word @Int16 32768 `shouldSatisfy` isLeft

    describe "TryCast Word Int32" $ do
      when (toInteger (maxBound :: Word) < 2147483647) untested
      test $ tryCast @Word @Int32 0 `shouldBe` Right 0
      test $ tryCast @Word @Int32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Word @Int32 2147483648 `shouldSatisfy` isLeft

    describe "TryCast Word Int64" $ do
      when (toInteger (maxBound :: Word) < 9223372036854775807) untested
      test $ tryCast @Word @Int64 0 `shouldBe` Right 0
      test $ tryCast @Word @Int64 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Word @Int32 9223372036854775808 `shouldSatisfy` isLeft

    describe "TryCast Word Int" $ do
      test $ tryCast @Word @Int 0 `shouldBe` Right 0
      test $ do
        let x = maxBound :: Int
        tryCast @Word @Int (fromIntegral x) `shouldBe` Right x
      test $ do
        let x = fromIntegral (maxBound :: Int) + 1 :: Word
        tryCast @Word @Int x `shouldSatisfy` isLeft

    describe "Cast Word Integer" $ do
      test $ cast @Word @Integer 0 `shouldBe` 0
      test $ cast @Word @Integer maxBound `shouldBe` fromIntegral (maxBound :: Word)

    describe "TryCast Word Float" $ do
      test $ tryCast @Word @Float 0 `shouldBe` Right 0
      test $ tryCast @Word @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Word @Float 16777216 `shouldSatisfy` isLeft

    describe "TryCast Word Double" $ do
      when (toInteger (maxBound :: Word) <= 9007199254740991) untested
      test $ tryCast @Word @Double 0 `shouldBe` Right 0
      test $ tryCast @Word @Double 9007199254740991 `shouldBe` Right 9007199254740991
      test $ tryCast @Word @Double 9007199254740992 `shouldSatisfy` isLeft

    -- Natural

    describe "TryCast Natural Word8" $ do
      test $ tryCast @Natural @Word8 0 `shouldBe` Right 0
      test $ tryCast @Natural @Word8 255 `shouldBe` Right 255
      test $ tryCast @Natural @Word8 256 `shouldSatisfy` isLeft

    describe "TryCast Natural Word16" $ do
      test $ tryCast @Natural @Word16 0 `shouldBe` Right 0
      test $ tryCast @Natural @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Natural @Word16 65536 `shouldSatisfy` isLeft

    describe "TryCast Natural Word32" $ do
      test $ tryCast @Natural @Word32 0 `shouldBe` Right 0
      test $ tryCast @Natural @Word32 4294967295 `shouldBe` Right 4294967295
      test $ tryCast @Natural @Word32 4294967296 `shouldSatisfy` isLeft

    describe "TryCast Natural Word64" $ do
      test $ tryCast @Natural @Word64 0 `shouldBe` Right 0
      test $ tryCast @Natural @Word64 18446744073709551615 `shouldBe` Right 18446744073709551615
      test $ tryCast @Natural @Word64 18446744073709551616 `shouldSatisfy` isLeft

    describe "TryCast Natural Word" $ do
      test $ tryCast @Natural @Word 0 `shouldBe` Right 0
      test $ do
        let x = maxBound :: Word
        tryCast @Natural @Word (fromIntegral x) `shouldBe` Right x
      test $ do
        let x = fromIntegral (maxBound :: Word) + 1 :: Natural
        tryCast @Natural @Word x `shouldSatisfy` isLeft

    describe "TryCast Natural Int8" $ do
      test $ tryCast @Natural @Int8 0 `shouldBe` Right 0
      test $ tryCast @Natural @Int8 127 `shouldBe` Right 127
      test $ tryCast @Natural @Int8 128 `shouldSatisfy` isLeft

    describe "TryCast Natural Int16" $ do
      test $ tryCast @Natural @Int16 0 `shouldBe` Right 0
      test $ tryCast @Natural @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Natural @Int16 32768 `shouldSatisfy` isLeft

    describe "TryCast Natural Int32" $ do
      test $ tryCast @Natural @Int32 0 `shouldBe` Right 0
      test $ tryCast @Natural @Int32 2147483647 `shouldBe` Right 2147483647
      test $ tryCast @Natural @Int32 2147483648 `shouldSatisfy` isLeft

    describe "TryCast Natural Int64" $ do
      test $ tryCast @Natural @Int64 0 `shouldBe` Right 0
      test $ tryCast @Natural @Int64 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ tryCast @Natural @Int32 9223372036854775808 `shouldSatisfy` isLeft

    describe "TryCast Natural Int" $ do
      test $ tryCast @Natural @Int 0 `shouldBe` Right 0
      test $ do
        let x = maxBound :: Int
        tryCast @Natural @Int (fromIntegral x) `shouldBe` Right x
      test $ do
        let x = fromIntegral (maxBound :: Int) + 1 :: Natural
        tryCast @Natural @Int x `shouldSatisfy` isLeft

    describe "Cast Natural Integer" $ do
      test $ cast @Natural @Integer 0 `shouldBe` 0
      test $ cast @Natural @Integer 9223372036854775808 `shouldBe` 9223372036854775808

    describe "TryCast Natural Float" $ do
      test $ tryCast @Natural @Float 0 `shouldBe` Right 0
      test $ tryCast @Natural @Float 16777215 `shouldBe` Right 16777215
      test $ tryCast @Natural @Float 16777216 `shouldSatisfy` isLeft

    describe "TryCast Natural Double" $ do
      test $ tryCast @Natural @Double 0 `shouldBe` Right 0
      test $ tryCast @Natural @Double 9007199254740991 `shouldBe` Right 9007199254740991
      test $ tryCast @Natural @Double 9007199254740992 `shouldSatisfy` isLeft

    -- Float

    describe "TryCast Float Int8" $ do
      test $ tryCast @Float @Int8 0 `shouldBe` Right 0
      test $ tryCast @Float @Int8 127 `shouldBe` Right 127
      test $ tryCast @Float @Int8 128 `shouldSatisfy` isLeft
      test $ tryCast @Float @Int8 (-128) `shouldBe` Right (-128)
      test $ tryCast @Float @Int8 (-129) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int8 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int8 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int8 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Int16" $ do
      test $ tryCast @Float @Int16 0 `shouldBe` Right 0
      test $ tryCast @Float @Int16 32767 `shouldBe` Right 32767
      test $ tryCast @Float @Int16 32768 `shouldSatisfy` isLeft
      test $ tryCast @Float @Int16 (-32768) `shouldBe` Right (-32768)
      test $ tryCast @Float @Int16 (-32769) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int16 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int16 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int16 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Int32" $ do
      test $ tryCast @Float @Int32 0 `shouldBe` Right 0
      test $ tryCast @Float @Int32 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Int32 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Int32 (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Float @Int32 (-16777216) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int32 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int32 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int32 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Int64" $ do
      test $ tryCast @Float @Int64 0 `shouldBe` Right 0
      test $ tryCast @Float @Int64 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Int64 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Int64 (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Float @Int64 (-16777216) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int64 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int64 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int64 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Int" $ do
      test $ tryCast @Float @Int 0 `shouldBe` Right 0
      test $ tryCast @Float @Int 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Int 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Int (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Float @Int (-16777216) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Int (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Integer" $ do
      test $ tryCast @Float @Integer 0 `shouldBe` Right 0
      test $ tryCast @Float @Integer 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Integer 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Integer (-16777215) `shouldBe` Right (-16777215)
      test $ tryCast @Float @Integer (-16777216) `shouldSatisfy` isLeft
      test $ tryCast @Float @Integer (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Integer (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Integer (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Word8" $ do
      test $ tryCast @Float @Word8 0 `shouldBe` Right 0
      test $ tryCast @Float @Word8 255 `shouldBe` Right 255
      test $ tryCast @Float @Word8 256 `shouldSatisfy` isLeft
      test $ tryCast @Float @Word8 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word8 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word8 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Word16" $ do
      test $ tryCast @Float @Word16 0 `shouldBe` Right 0
      test $ tryCast @Float @Word16 65535 `shouldBe` Right 65535
      test $ tryCast @Float @Word16 65536 `shouldSatisfy` isLeft
      test $ tryCast @Float @Word16 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word16 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word16 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Word32" $ do
      test $ tryCast @Float @Word32 0 `shouldBe` Right 0
      test $ tryCast @Float @Word32 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Word32 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Word32 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word32 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word32 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Word64" $ do
      test $ tryCast @Float @Word64 0 `shouldBe` Right 0
      test $ tryCast @Float @Word64 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Word64 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Word64 (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word64 (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word64 (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Word" $ do
      test $ tryCast @Float @Word 0 `shouldBe` Right 0
      test $ tryCast @Float @Word 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Word 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Word (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Word (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Natural" $ do
      test $ tryCast @Float @Natural 0 `shouldBe` Right 0
      test $ tryCast @Float @Natural 16777215 `shouldBe` Right 16777215
      test $ tryCast @Float @Natural 16777216 `shouldSatisfy` isLeft
      test $ tryCast @Float @Natural (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Natural (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Natural (-1 / 0) `shouldSatisfy` isLeft

    describe "TryCast Float Rational" $ do
      test $ tryCast @Float @Rational 0 `shouldBe` Right 0
      -- TODO
      test $ tryCast @Float @Rational (0 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Rational (1 / 0) `shouldSatisfy` isLeft
      test $ tryCast @Float @Rational (-1 / 0) `shouldSatisfy` isLeft

    -- NonEmpty

    describe "TryCast [a] (NonEmpty a)" $ do
      test $ tryCast @[Int] @(NonEmpty Int) [] `shouldSatisfy` isLeft
      test $ tryCast @[Int] @(NonEmpty Int) [1] `shouldBe` Right (1 :| [])
      test $ tryCast @[Int] @(NonEmpty Int) [1, 2] `shouldBe` Right (1 :| [2])

    describe "Cast (NonEmpty a) [a]" $ do
      test $ cast @(NonEmpty Int) @[Int] (1 :| []) `shouldBe` [1]
      test $ cast @(NonEmpty Int) @[Int] (1 :| [2]) `shouldBe` [1, 2]

    -- Ratio

    describe "Cast a (Ratio a)" $ do
      test $ cast @Integer @Rational 0 `shouldBe` 0
      test $ cast @Int @(Ratio Int) 0 `shouldBe` 0

    describe "TryCast (Ratio a) a" $ do
      test $ tryCast @Rational @Integer 0 `shouldBe` Right 0
      test $ tryCast @Rational @Integer 0.5 `shouldSatisfy` isLeft
      test $ tryCast @(Ratio Int) @Int 0 `shouldBe` Right 0
      test $ tryCast @(Ratio Int) @Int 0.5 `shouldSatisfy` isLeft

    -- Fixed

    describe "Cast Integer (Fixed a)" $ do
      test $ cast @Integer @Uni 1 `shouldBe` 1
      test $ cast @Integer @Deci 1 `shouldBe` 0.1

    describe "Cast (Fixed a) Integer" $ do
      test $ cast @Uni @Integer 1 `shouldBe` 1
      test $ cast @Deci @Integer 1 `shouldBe` 10

    -- Complex

    describe "Cast a (Complex a)" $ do
      test $ cast @Float @(Complex Float) 1 `shouldBe` 1
      test $ cast @Double @(Complex Double) 1 `shouldBe` 1

    describe "TryCast (Complex a) a" $ do
      test $ tryCast @(Complex Float) @Float 1 `shouldBe` Right 1
      test $ tryCast @(Complex Float) @Float (0 :+ 1) `shouldSatisfy` isLeft
      test $ tryCast @(Complex Double) @Double 1 `shouldBe` Right 1
      test $ tryCast @(Complex Double) @Double (0 :+ 1) `shouldSatisfy` isLeft

test :: Example a => a -> SpecWith (Arg a)
test = it ""

untested :: SpecWith a
untested = runIO $ throwIO Untested

data Untested
  = Untested
  deriving (Eq, Show)

instance Exception Untested

newtype Name
  = Name String
  deriving (Eq, Show)

instance Cast Name String

instance Cast String Name
