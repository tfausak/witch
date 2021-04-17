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
import Test.QuickCheck
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
      let f = cast @Int8 @Int16
      test $ f 0 `shouldBe` 0
      test $ f 127 `shouldBe` 127
      test $ f (-128) `shouldBe` (-128)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int8 Int32" $ do
      let f = cast @Int8 @Int32
      test $ f 0 `shouldBe` 0
      test $ f 127 `shouldBe` 127
      test $ f (-128) `shouldBe` (-128)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int8 Int64" $ do
      let f = cast @Int8 @Int64
      test $ f 0 `shouldBe` 0
      test $ f 127 `shouldBe` 127
      test $ f (-128) `shouldBe` (-128)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int8 Int" $ do
      let f = cast @Int8 @Int
      test $ f 0 `shouldBe` 0
      test $ f 127 `shouldBe` 127
      test $ f (-128) `shouldBe` (-128)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int8 Integer" $ do
      let f = cast @Int8 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 127 `shouldBe` 127
      test $ f (-128) `shouldBe` (-128)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int8 Word8" $ do
      let f = tryCast @Int8 @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int8 Word16" $ do
      let f = tryCast @Int8 @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int8 Word32" $ do
      let f = tryCast @Int8 @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int8 Word64" $ do
      let f = tryCast @Int8 @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int8 Word" $ do
      let f = tryCast @Int8 @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int8 Natural" $ do
      let f = tryCast @Int8 @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int8 Float" $ do
      let f = cast @Int8 @Float
      test $ f 0 `shouldBe` 0
      test $ f 127 `shouldBe` 127
      test $ f (-128) `shouldBe` (-128)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int8 Double" $ do
      let f = cast @Int8 @Double
      test $ f 0 `shouldBe` 0
      test $ f 127 `shouldBe` 127
      test $ f (-128) `shouldBe` (-128)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Int16

    describe "TryCast Int16 Int8" $ do
      let f = tryCast @Int16 @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      test $ f (-128) `shouldBe` Right (-128)
      test $ f (-129) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int16 Int32" $ do
      let f = cast @Int16 @Int32
      test $ f 0 `shouldBe` 0
      test $ f 32767 `shouldBe` 32767
      test $ f (-32768) `shouldBe` (-32768)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int16 Int64" $ do
      let f = cast @Int16 @Int64
      test $ f 0 `shouldBe` 0
      test $ f 32767 `shouldBe` 32767
      test $ f (-32768) `shouldBe` (-32768)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int16 Int" $ do
      let f = cast @Int16 @Int
      test $ f 0 `shouldBe` 0
      test $ f 32767 `shouldBe` 32767
      test $ f (-32768) `shouldBe` (-32768)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int16 Integer" $ do
      let f = cast @Int16 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 32767 `shouldBe` 32767
      test $ f (-32768) `shouldBe` (-32768)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int16 Word8" $ do
      let f = tryCast @Int16 @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int16 Word16" $ do
      let f = tryCast @Int16 @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int16 Word32" $ do
      let f = tryCast @Int16 @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int16 Word64" $ do
      let f = tryCast @Int16 @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int16 Word" $ do
      let f = tryCast @Int16 @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int16 Natural" $ do
      let f = tryCast @Int16 @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int16 Float" $ do
      let f = cast @Int16 @Float
      test $ f 0 `shouldBe` 0
      test $ f 32767 `shouldBe` 32767
      test $ f (-32768) `shouldBe` (-32768)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int16 Double" $ do
      let f = cast @Int16 @Double
      test $ f 0 `shouldBe` 0
      test $ f 32767 `shouldBe` 32767
      test $ f (-32768) `shouldBe` (-32768)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Int32

    describe "TryCast Int32 Int8" $ do
      let f = tryCast @Int32 @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      test $ f (-128) `shouldBe` Right (-128)
      test $ f (-129) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Int16" $ do
      let f = tryCast @Int32 @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      test $ f (-32768) `shouldBe` Right (-32768)
      test $ f (-32769) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int32 Int64" $ do
      let f = cast @Int32 @Int64
      test $ f 0 `shouldBe` 0
      test $ f 2147483647 `shouldBe` 2147483647
      test $ f (-2147483648) `shouldBe` (-2147483648)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Int" $ do
      when (toInteger (maxBound :: Int) < 2147483647) untested
      let f = tryCast @Int32 @Int
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f (-2147483648) `shouldBe` Right (-2147483648)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int32 Integer" $ do
      let f = cast @Int32 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 2147483647 `shouldBe` 2147483647
      test $ f (-2147483648) `shouldBe` (-2147483648)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Word8" $ do
      let f = tryCast @Int32 @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Word16" $ do
      let f = tryCast @Int32 @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Word32" $ do
      let f = tryCast @Int32 @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Word64" $ do
      let f = tryCast @Int32 @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Word" $ do
      when (toInteger (maxBound :: Word) < 2147483647) untested
      let f = tryCast @Int32 @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Natural" $ do
      let f = tryCast @Int32 @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int32 Float" $ do
      let f = tryCast @Int32 @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int32 Double" $ do
      let f = cast @Int32 @Double
      test $ f 0 `shouldBe` 0
      test $ f 2147483647 `shouldBe` 2147483647
      test $ f (-2147483648) `shouldBe` (-2147483648)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Int64

    describe "TryCast Int64 Int8" $ do
      let f = tryCast @Int64 @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      test $ f (-128) `shouldBe` Right (-128)
      test $ f (-129) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Int16" $ do
      let f = tryCast @Int64 @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      test $ f (-32768) `shouldBe` Right (-32768)
      test $ f (-32769) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Int32" $ do
      let f = tryCast @Int64 @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      test $ f (-2147483648) `shouldBe` Right (-2147483648)
      test $ f (-2147483649) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Int" $ do
      when (toInteger (maxBound :: Int) < 9223372036854775807) untested
      let f = tryCast @Int64 @Int
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f (-9223372036854775808) `shouldBe` Right (-9223372036854775808)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int64 Integer" $ do
      let f = cast @Int64 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 9223372036854775807 `shouldBe` 9223372036854775807
      test $ f (-9223372036854775808) `shouldBe` (-9223372036854775808)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Word8" $ do
      let f = tryCast @Int64 @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Word16" $ do
      let f = tryCast @Int64 @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Word32" $ do
      let f = tryCast @Int64 @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Word64" $ do
      let f = tryCast @Int64 @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Word" $ do
      when (toInteger (maxBound :: Word) < 9223372036854775807) untested
      let f = tryCast @Int64 @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Natural" $ do
      let f = tryCast @Int64 @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Float" $ do
      let f = tryCast @Int64 @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int64 Double" $ do
      let f = tryCast @Int64 @Double
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ f (-9007199254740992) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Int

    describe "TryCast Int Int8" $ do
      let f = tryCast @Int @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      test $ f (-128) `shouldBe` Right (-128)
      test $ f (-129) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Int16" $ do
      let f = tryCast @Int @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      test $ f (-32768) `shouldBe` Right (-32768)
      test $ f (-32769) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Int32" $ do
      when (toInteger (maxBound :: Int) < 2147483647) untested
      let f = tryCast @Int @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      test $ f (-2147483648) `shouldBe` Right (-2147483648)
      test $ f (-2147483649) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int Int64" $ do
      let f = cast @Int @Int64
      test $ f 0 `shouldBe` 0
      test $ f maxBound `shouldBe` fromIntegral (maxBound :: Int)
      test $ f minBound `shouldBe` fromIntegral (minBound :: Int)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Int Integer" $ do
      let f = cast @Int @Integer
      test $ f 0 `shouldBe` 0
      test $ f maxBound `shouldBe` fromIntegral (maxBound :: Int)
      test $ f minBound `shouldBe` fromIntegral (minBound :: Int)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Word8" $ do
      let f = tryCast @Int @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Word16" $ do
      let f = tryCast @Int @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Word32" $ do
      when (toInteger (maxBound :: Int) < 4294967295) untested
      let f = tryCast @Int @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      test $ f 4294967296 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Word64" $ do
      let f = tryCast @Int @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f maxBound `shouldBe` Right (fromIntegral (maxBound :: Int))
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Word" $ do
      let f = tryCast @Int @Word
      test $ f 0 `shouldBe` Right 0
      test $ f maxBound `shouldBe` Right (fromIntegral (maxBound :: Int))
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Natural" $ do
      let f = tryCast @Int @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f maxBound `shouldBe` Right (fromIntegral (maxBound :: Int))
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Float" $ do
      let f = tryCast @Int @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Int Double" $ do
      when (toInteger (maxBound :: Int) < 9007199254740991) untested
      let f = tryCast @Int @Double
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ f (-9007199254740992) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Integer

    describe "TryCast Integer Int8" $ do
      let f = tryCast @Integer @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      test $ f (-128) `shouldBe` Right (-128)
      test $ f (-129) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Int16" $ do
      let f = tryCast @Integer @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      test $ f (-32768) `shouldBe` Right (-32768)
      test $ f (-32769) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Int32" $ do
      let f = tryCast @Integer @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      test $ f (-2147483648) `shouldBe` Right (-2147483648)
      test $ f (-2147483649) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Int64" $ do
      let f = tryCast @Integer @Int64
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f 9223372036854775808 `shouldSatisfy` isLeft
      test $ f (-9223372036854775808) `shouldBe` Right (-9223372036854775808)
      test $ f (-9223372036854775809) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Int" $ do
      let f = tryCast @Integer @Int
      test $ f 0 `shouldBe` Right 0
      test $ let x = maxBound :: Int in f (fromIntegral x) `shouldBe` Right x
      test $ let x = toInteger (maxBound :: Int) + 1 in f x `shouldSatisfy` isLeft
      test $ let x = minBound :: Int in f (fromIntegral x) `shouldBe` Right x
      test $ let x = toInteger (minBound :: Int) - 1 in f x `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Word8" $ do
      let f = tryCast @Integer @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Word16" $ do
      let f = tryCast @Integer @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Word32" $ do
      let f = tryCast @Integer @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      test $ f 4294967296 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Word64" $ do
      let f = tryCast @Integer @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 18446744073709551615 `shouldBe` Right 18446744073709551615
      test $ f 18446744073709551616 `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Word" $ do
      let f = tryCast @Integer @Word
      test $ f 0 `shouldBe` Right 0
      test $ let x = maxBound :: Word in f (fromIntegral x) `shouldBe` Right x
      test $ let x = toInteger (maxBound :: Word) + 1 in f x `shouldSatisfy` isLeft
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Natural" $ do
      let f = tryCast @Integer @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f 18446744073709551616 `shouldBe` Right 18446744073709551616
      test $ f (-1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Float" $ do
      let f = tryCast @Integer @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Integer Double" $ do
      let f = tryCast @Integer @Double
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ f (-9007199254740992) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Word8

    describe "Cast Word8 Word16" $ do
      let f = cast @Word8 @Word16
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Word32" $ do
      let f = cast @Word8 @Word32
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Word64" $ do
      let f = cast @Word8 @Word64
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Word" $ do
      let f = cast @Word8 @Word
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Natural" $ do
      let f = cast @Word8 @Natural
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word8 Int8" $ do
      let f = tryCast @Word8 @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Int16" $ do
      let f = cast @Word8 @Int16
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Int32" $ do
      let f = cast @Word8 @Int32
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Int64" $ do
      let f = cast @Word8 @Int64
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Int" $ do
      let f = cast @Word8 @Int
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Integer" $ do
      let f = cast @Word8 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Float" $ do
      let f = cast @Word8 @Float
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word8 Double" $ do
      let f = cast @Word8 @Double
      test $ f 0 `shouldBe` 0
      test $ f 255 `shouldBe` 255
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Word16

    describe "TryCast Word16 Word8" $ do
      let f = tryCast @Word16 @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Word32" $ do
      let f = cast @Word16 @Word32
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Word64" $ do
      let f = cast @Word16 @Word64
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Word" $ do
      let f = cast @Word16 @Word
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Natural" $ do
      let f = cast @Word16 @Natural
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word16 Int8" $ do
      let f = tryCast @Word16 @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word16 Int16" $ do
      let f = tryCast @Word16 @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Int32" $ do
      let f = cast @Word16 @Int32
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Int64" $ do
      let f = cast @Word16 @Int64
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Int" $ do
      let f = cast @Word16 @Int
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Integer" $ do
      let f = cast @Word16 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Float" $ do
      let f = cast @Word16 @Float
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word16 Double" $ do
      let f = cast @Word16 @Double
      test $ f 0 `shouldBe` 0
      test $ f 65535 `shouldBe` 65535
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Word32

    describe "TryCast Word32 Word8" $ do
      let f = tryCast @Word32 @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word32 Word16" $ do
      let f = tryCast @Word32 @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word32 Word64" $ do
      let f = cast @Word32 @Word64
      test $ f 0 `shouldBe` 0
      test $ f 4294967295 `shouldBe` 4294967295
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word32 Word" $ do
      when (toInteger (maxBound :: Word) < 4294967295) untested
      let f = tryCast @Word32 @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word32 Natural" $ do
      let f = cast @Word32 @Natural
      test $ f 0 `shouldBe` 0
      test $ f 4294967295 `shouldBe` 4294967295
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word32 Int8" $ do
      let f = tryCast @Word32 @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word32 Int16" $ do
      let f = tryCast @Word32 @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word32 Int32" $ do
      let f = tryCast @Word32 @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word32 Int64" $ do
      let f = cast @Word32 @Int64
      test $ f 0 `shouldBe` 0
      test $ f 4294967295 `shouldBe` 4294967295
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word32 Int" $ do
      when (toInteger (maxBound :: Int) < 4294967295) untested
      let f = tryCast @Word32 @Int
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word32 Integer" $ do
      let f = cast @Word32 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 4294967295 `shouldBe` 4294967295
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word32 Float" $ do
      let f = tryCast @Word32 @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word32 Double" $ do
      let f = cast @Word32 @Double
      test $ f 0 `shouldBe` 0
      test $ f 4294967295 `shouldBe` 4294967295
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Word64

    describe "TryCast Word64 Word8" $ do
      let f = tryCast @Word64 @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Word16" $ do
      let f = tryCast @Word64 @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Word32" $ do
      let f = tryCast @Word64 @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      test $ f 4294967296 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Word" $ do
      when (toInteger (maxBound :: Word) < 18446744073709551615) untested
      let f = tryCast @Word64 @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 18446744073709551615 `shouldBe` Right 18446744073709551615
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word64 Natural" $ do
      let f = cast @Word64 @Natural
      test $ f 0 `shouldBe` 0
      test $ f 18446744073709551615 `shouldBe` 18446744073709551615
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Int8" $ do
      let f = tryCast @Word64 @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Int16" $ do
      let f = tryCast @Word64 @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Int32" $ do
      let f = tryCast @Word64 @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Int64" $ do
      let f = tryCast @Word64 @Int64
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f 9223372036854775808 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Int" $ do
      let f = tryCast @Word64 @Int
      test $ f 0 `shouldBe` Right 0
      test $ let x = maxBound :: Int in tryCast @Word64 @Int (fromIntegral x) `shouldBe` Right x
      test $ let x = fromIntegral (maxBound :: Int) + 1 :: Word64 in tryCast @Word64 @Int x `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word64 Integer" $ do
      let f = cast @Word64 @Integer
      test $ f 0 `shouldBe` 0
      test $ f 18446744073709551615 `shouldBe` 18446744073709551615
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Float" $ do
      let f = tryCast @Word64 @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word64 Double" $ do
      let f = tryCast @Word64 @Double
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Word

    describe "TryCast Word Word8" $ do
      let f = tryCast @Word @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Word16" $ do
      let f = tryCast @Word @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Word32" $ do
      when (toInteger (maxBound :: Word) < 4294967295) untested
      let f = tryCast @Word @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      test $ f 4294967296 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word Word64" $ do
      let f = cast @Word @Word64
      test $ f 0 `shouldBe` 0
      test $ f maxBound `shouldBe` fromIntegral (maxBound :: Word)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word Natural" $ do
      let f = cast @Word @Natural
      test $ f 0 `shouldBe` 0
      test $ f maxBound `shouldBe` fromIntegral (maxBound :: Word)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Int8" $ do
      let f = tryCast @Word @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Int16" $ do
      let f = tryCast @Word @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Int32" $ do
      when (toInteger (maxBound :: Word) < 2147483647) untested
      let f = tryCast @Word @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Int64" $ do
      when (toInteger (maxBound :: Word) < 9223372036854775807) untested
      let f = tryCast @Word @Int64
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f 9223372036854775808 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Int" $ do
      let f = tryCast @Word @Int
      test $ f 0 `shouldBe` Right 0
      test $ let x = maxBound :: Int in tryCast @Word @Int (fromIntegral x) `shouldBe` Right x
      test $ let x = fromIntegral (maxBound :: Int) + 1 :: Word in tryCast @Word @Int x `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Word Integer" $ do
      let f = cast @Word @Integer
      test $ f 0 `shouldBe` 0
      test $ f maxBound `shouldBe` fromIntegral (maxBound :: Word)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Float" $ do
      let f = tryCast @Word @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Word Double" $ do
      when (toInteger (maxBound :: Word) < 9007199254740991) untested
      let f = tryCast @Word @Double
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Natural

    describe "TryCast Natural Word8" $ do
      let f = tryCast @Natural @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Word16" $ do
      let f = tryCast @Natural @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Word32" $ do
      let f = tryCast @Natural @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      test $ f 4294967296 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Word64" $ do
      let f = tryCast @Natural @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 18446744073709551615 `shouldBe` Right 18446744073709551615
      test $ f 18446744073709551616 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Word" $ do
      let f = tryCast @Natural @Word
      test $ f 0 `shouldBe` Right 0
      test $ let x = maxBound :: Word in tryCast @Natural @Word (fromIntegral x) `shouldBe` Right x
      test $ let x = fromIntegral (maxBound :: Word) + 1 :: Natural in tryCast @Natural @Word x `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Int8" $ do
      let f = tryCast @Natural @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Int16" $ do
      let f = tryCast @Natural @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Int32" $ do
      let f = tryCast @Natural @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Int64" $ do
      let f = tryCast @Natural @Int64
      test $ f 0 `shouldBe` Right 0
      test $ f 9223372036854775807 `shouldBe` Right 9223372036854775807
      test $ f 9223372036854775808 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Int" $ do
      let f = tryCast @Natural @Int
      test $ f 0 `shouldBe` Right 0
      test $ let x = maxBound :: Int in tryCast @Natural @Int (fromIntegral x) `shouldBe` Right x
      test $ let x = fromIntegral (maxBound :: Int) + 1 :: Natural in tryCast @Natural @Int x `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Natural Integer" $ do
      let f = cast @Natural @Integer
      test $ f 0 `shouldBe` 0
      test $ f 9223372036854775808 `shouldBe` 9223372036854775808
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Float" $ do
      let f = tryCast @Natural @Float
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Natural Double" $ do
      let f = tryCast @Natural @Double
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Float

    describe "TryCast Float Int8" $ do
      let f = tryCast @Float @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      test $ f (-128) `shouldBe` Right (-128)
      test $ f (-129) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Int16" $ do
      let f = tryCast @Float @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      test $ f (-32768) `shouldBe` Right (-32768)
      test $ f (-32769) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Int32" $ do
      let f = tryCast @Float @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Int64" $ do
      let f = tryCast @Float @Int64
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Int" $ do
      let f = tryCast @Float @Int
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Integer" $ do
      let f = tryCast @Float @Integer
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f (-16777216) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Word8" $ do
      let f = tryCast @Float @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Word16" $ do
      let f = tryCast @Float @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Word32" $ do
      let f = tryCast @Float @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Word64" $ do
      let f = tryCast @Float @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Word" $ do
      let f = tryCast @Float @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Natural" $ do
      let f = tryCast @Float @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f 16777216 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Float Rational" $ do
      let f = tryCast @Float @Rational
      test $ f 0 `shouldBe` Right 0
      test $ f (-0) `shouldBe` Right 0
      test $ f 0.5 `shouldBe` Right 0.5
      test $ f (-0.5) `shouldBe` Right (-0.5)
      test $ f 16777215 `shouldBe` Right 16777215
      test $ f (-16777215) `shouldBe` Right (-16777215)
      test $ f 16777216 `shouldBe` Right 16777216
      test $ f (-16777216) `shouldBe` Right (-16777216)
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast Float Double" $ do
      let f = cast @Float @Double
      test $ f 0 `shouldBe` 0
      test $ f 0.5 `shouldBe` 0.5
      test $ f (-0.5) `shouldBe` (-0.5)
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Float

    describe "TryCast Double Int8" $ do
      let f = tryCast @Double @Int8
      test $ f 0 `shouldBe` Right 0
      test $ f 127 `shouldBe` Right 127
      test $ f 128 `shouldSatisfy` isLeft
      test $ f (-128) `shouldBe` Right (-128)
      test $ f (-129) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Int16" $ do
      let f = tryCast @Double @Int16
      test $ f 0 `shouldBe` Right 0
      test $ f 32767 `shouldBe` Right 32767
      test $ f 32768 `shouldSatisfy` isLeft
      test $ f (-32768) `shouldBe` Right (-32768)
      test $ f (-32769) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Int32" $ do
      let f = tryCast @Double @Int32
      test $ f 0 `shouldBe` Right 0
      test $ f 2147483647 `shouldBe` Right 2147483647
      test $ f 2147483648 `shouldSatisfy` isLeft
      test $ f (-2147483648) `shouldBe` Right (-2147483648)
      test $ f (-2147483649) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Int64" $ do
      let f = tryCast @Double @Int64
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ f (-9007199254740992) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Int" $ do
      when (toInteger (maxBound :: Int) < 9007199254740991) untested
      let f = tryCast @Double @Int
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ f (-9007199254740992) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Integer" $ do
      let f = tryCast @Double @Integer
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ f (-9007199254740992) `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Word8" $ do
      let f = tryCast @Double @Word8
      test $ f 0 `shouldBe` Right 0
      test $ f 255 `shouldBe` Right 255
      test $ f 256 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Word16" $ do
      let f = tryCast @Double @Word16
      test $ f 0 `shouldBe` Right 0
      test $ f 65535 `shouldBe` Right 65535
      test $ f 65536 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Word32" $ do
      let f = tryCast @Double @Word32
      test $ f 0 `shouldBe` Right 0
      test $ f 4294967295 `shouldBe` Right 4294967295
      test $ f 4294967296 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Word64" $ do
      let f = tryCast @Double @Word64
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Word" $ do
      when (toInteger (maxBound :: Word) < 9007199254740991) untested
      let f = tryCast @Double @Word
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Natural" $ do
      let f = tryCast @Double @Natural
      test $ f 0 `shouldBe` Right 0
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f 9007199254740992 `shouldSatisfy` isLeft
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast Double Rational" $ do
      let f = tryCast @Double @Rational
      test $ f 0 `shouldBe` Right 0
      test $ f (-0) `shouldBe` Right 0
      test $ f 0.5 `shouldBe` Right 0.5
      test $ f (-0.5) `shouldBe` Right (-0.5)
      test $ f 9007199254740991 `shouldBe` Right 9007199254740991
      test $ f (-9007199254740991) `shouldBe` Right (-9007199254740991)
      test $ f 9007199254740992 `shouldBe` Right 9007199254740992
      test $ f (-9007199254740992) `shouldBe` Right (-9007199254740992)
      test $ f (0 / 0) `shouldSatisfy` isLeft
      test $ f (1 / 0) `shouldSatisfy` isLeft
      test $ f (-1 / 0) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- NonEmpty

    describe "TryCast [a] (NonEmpty a)" $ do
      let f = tryCast @[Int] @(NonEmpty Int)
      test $ f [] `shouldSatisfy` isLeft
      test $ f [1] `shouldBe` Right (1 :| [])
      test $ f [1, 2] `shouldBe` Right (1 :| [2])
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast (NonEmpty a) [a]" $ do
      let f = cast @(NonEmpty Int) @[Int]
      test $ f (1 :| []) `shouldBe` [1]
      test $ f (1 :| [2]) `shouldBe` [1, 2]
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Ratio

    describe "Cast a (Ratio a)" $ do
      test $ cast @Integer @Rational 0 `shouldBe` 0
      let f = cast @Int @(Ratio Int)
      test $ f 0 `shouldBe` 0
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast (Ratio a) a" $ do
      test $ tryCast @Rational @Integer 0 `shouldBe` Right 0
      test $ tryCast @Rational @Integer 0.5 `shouldSatisfy` isLeft
      let f = tryCast @(Ratio Int) @Int
      test $ f 0 `shouldBe` Right 0
      test $ f 0.5 `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Fixed

    describe "Cast Integer (Fixed a)" $ do
      test $ cast @Integer @Uni 1 `shouldBe` 1
      let f = cast @Integer @Deci
      test $ f 1 `shouldBe` 0.1
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "Cast (Fixed a) Integer" $ do
      test $ cast @Uni @Integer 1 `shouldBe` 1
      let f = cast @Deci @Integer
      test $ f 1 `shouldBe` 10
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    -- Complex

    describe "Cast a (Complex a)" $ do
      test $ cast @Double @(Complex Double) 1 `shouldBe` 1
      let f = cast @Float @(Complex Float)
      test $ f 1 `shouldBe` 1
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

    describe "TryCast (Complex a) a" $ do
      test $ tryCast @(Complex Double) @Double 1 `shouldBe` Right 1
      test $ tryCast @(Complex Double) @Double (0 :+ 1) `shouldSatisfy` isLeft
      let f = tryCast @(Complex Float) @Float
      test $ f 1 `shouldBe` Right 1
      test $ f (0 :+ 1) `shouldSatisfy` isLeft
      prop $ \ x y -> if x == y then f x == f y else f x /= f y

test :: Example a => a -> SpecWith (Arg a)
test = it ""

prop :: Testable a => a -> Spec
prop = test . property

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

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
