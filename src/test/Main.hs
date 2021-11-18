{-# OPTIONS_GHC -Wno-error=overflowed-literals #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.Complex as Complex
import qualified Data.Either as Either
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
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Time.Clock.TAI as Time
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import Test.HUnit (Test(TestCase), assertBool, runTestTTAndExit, (~:), (~?=))
import qualified Witch

main :: IO ()
main = runTestTTAndExit $ "Witch" ~:
  [ "From" ~:
    [ "from" ~:
      [ Witch.from (1 :: Int.Int8) ~?= (1 :: Int.Int16)
      ]
    ]
  , "TryFrom" ~:
    [ "tryFrom" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Int.Int8 in
      [ f 1 ~?= Just 1
      , f 128 ~?= Nothing
      ]
    ]
  , "Utility" ~:
    [ "as" ~:
      [ Witch.as @Int.Int8 1 ~?= 1
      ]
    , "from" ~:
      [ Witch.from @Int.Int8 1 ~?= (1 :: Int.Int16)
      ]
    , "into" ~:
      [ Witch.into @Int.Int16 (1 :: Int.Int8) ~?= 1
      ]
    , "over" ~:
      [ Witch.over @Int.Int8 (+ 1) (Age 1) ~?= Age 2
      ]
    , "via" ~:
      [ Witch.via @Int.Int16 (1 :: Int.Int8) ~?= (1 :: Int.Int32)
      ]
    , "tryFrom" ~:
      [ hush (Witch.tryFrom @Int.Int16 1) ~?= Just (1 :: Int.Int8)
      ]
    , "tryInto" ~:
      [ hush (Witch.tryInto @Int.Int8 (1 :: Int.Int16)) ~?= Just 1
      ]
    , "tryVia" ~:
      let f = Witch.tryVia @Int.Int16 @Int.Int32 @Int.Int8 in
      [ hush (f 1) ~?= Just 1
      , hush (f 128) ~?= Nothing
      , hush (f 32768) ~?= Nothing
      ]
    , "unsafeFrom" ~:
      let f = Witch.unsafeFrom @Int.Int16 @Int.Int8 in
      [ f 1 ~?= 1
      , TestCase $ do
        result <- Exception.try @Exception.SomeException . Exception.evaluate $ f 128
        assertBool (show result) $ Either.isLeft result
      ]
    , "unsafeInto" ~:
      [ Witch.unsafeInto @Int.Int8 (1 :: Int.Int16) ~?= 1
      ]
    ]
  , "Lift" ~:
    [ "liftedFrom" ~:
      [ ($$(Witch.liftedFrom (1 :: Int.Int16)) :: Int.Int8) ~?= 1
      ]
    , "liftedInto" ~:
      [ $$(Witch.liftedInto @Int.Int8 (1 :: Int.Int16)) ~?= 1
      ]
    ]
  , "TryFromException" ~:
    [ "show" ~:
      [ show (Witch.TryFromException @Int @Int 0 Nothing) ~?= "TryFromException @Int @Int 0 Nothing"
      , show (Witch.TryFromException @(Seq.Seq Int) @(Seq.Seq Int) (Seq.fromList []) (Just (Exception.toException Exception.Overflow))) ~?= "TryFromException @(Seq Int) @(Seq Int) (fromList []) (Just arithmetic overflow)"
      ]
    ]
  , "Instances" ~:
    [ "From a a" ~:
      let f = Witch.from @Int @Int in
      [ f 0 ~?= 0
      ]

    -- Int8

    , "From Int8 Int16" ~:
      let f = Witch.from @Int.Int8 @Int.Int16 in
      [ f 0 ~?= 0
      , f 127 ~?= 127
      , f (-128) ~?= (-128)
      ]
    , "From Int8 Int32" ~:
      let f = Witch.from @Int.Int8 @Int.Int32 in
      [ f 0 ~?= 0
      , f 127 ~?= 127
      , f (-128) ~?= (-128)
      ]
    , "From Int8 Int64" ~:
      let f = Witch.from @Int.Int8 @Int.Int64 in
      [ f 0 ~?= 0
      , f 127 ~?= 127
      , f (-128) ~?= (-128)
      ]
    , "From Int8 Int" ~:
      let f = Witch.from @Int.Int8 @Int in
      [ f 0 ~?= 0
      , f 127 ~?= 127
      , f (-128) ~?= (-128)
      ]
    , "From Int8 Integer" ~:
      let f = Witch.from @Int.Int8 @Integer in
      [ f 0 ~?= 0
      , f 127 ~?= 127
      , f (-128) ~?= (-128)
      ]
    , "TryFrom Int8 Word8" ~:
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int8 Word16" ~:
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int8 Word32" ~:
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int8 Word64" ~:
      let f = hush . Witch.tryFrom @Int.Int8 @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int8 Word" ~:
      let f = hush . Witch.tryFrom @Int.Int8 @Word in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int8 Natural" ~:
      let f = hush . Witch.tryFrom @Int.Int8 @Natural.Natural in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f (-1) ~?= Nothing
      ]
    , "From Int8 Float" ~:
      let f = Witch.from @Int.Int8 @Float in
      [ f 0 ~?= 0
      , f 127 ~?= 127
      , f (-128) ~?= (-128)
      ]
    , "From Int8 Double" ~:
      let f = Witch.from @Int.Int8 @Double in
      [ f 0 ~?= 0
      , f 127 ~?= 127
      , f (-128) ~?= (-128)
      ]

    -- Int16

    , "TryFrom Int16 Int8" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      , f (-128) ~?= Just (-128)
      , f (-129) ~?= Nothing
      ]
    , "From Int16 Int32" ~:
      let f = Witch.from @Int.Int16 @Int.Int32 in
      [ f 0 ~?= 0
      , f 32767 ~?= 32767
      , f (-32768) ~?= (-32768)
      ]
    , "From Int16 Int64" ~:
      let f = Witch.from @Int.Int16 @Int.Int64 in
      [ f 0 ~?= 0
      , f 32767 ~?= 32767
      , f (-32768) ~?= (-32768)
      ]
    , "From Int16 Int" ~:
      let f = Witch.from @Int.Int16 @Int in
      [ f 0 ~?= 0
      , f 32767 ~?= 32767
      , f (-32768) ~?= (-32768)
      ]
    , "From Int16 Integer" ~:
      let f = Witch.from @Int.Int16 @Integer in
      [ f 0 ~?= 0
      , f 32767 ~?= 32767
      , f (-32768) ~?= (-32768)
      ]
    , "TryFrom Int16 Word8" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int16 Word16" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int16 Word32" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int16 Word64" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int16 Word" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Word in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int16 Natural" ~:
      let f = hush . Witch.tryFrom @Int.Int16 @Natural.Natural in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f (-1) ~?= Nothing
      ]
    , "From Int16 Float" ~:
      let f = Witch.from @Int.Int16 @Float in
      [ f 0 ~?= 0
      , f 32767 ~?= 32767
      , f (-32768) ~?= (-32768)
      ]
    , "From Int16 Double" ~:
      let f = Witch.from @Int.Int16 @Double in
      [ f 0 ~?= 0
      , f 32767 ~?= 32767
      , f (-32768) ~?= (-32768)
      ]

    -- Int32

    , "TryFrom Int32 Int8" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      , f (-128) ~?= Just (-128)
      , f (-129) ~?= Nothing
      ]
    , "TryFrom Int32 Int16" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      , f (-32768) ~?= Just (-32768)
      , f (-32769) ~?= Nothing
      ]
    , "From Int32 Int64" ~:
      let f = Witch.from @Int.Int32 @Int.Int64 in
      [ f 0 ~?= 0
      , f 2147483647 ~?= 2147483647
      , f (-2147483648) ~?= (-2147483648)
      ]
    , "TryFrom Int32 Int" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Int in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f (-2147483648) ~?= Just (-2147483648)
      ]
    , "From Int32 Integer" ~:
      let f = Witch.from @Int.Int32 @Integer in
      [ f 0 ~?= 0
      , f 2147483647 ~?= 2147483647
      , f (-2147483648) ~?= (-2147483648)
      ]
    , "TryFrom Int32 Word8" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int32 Word16" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int32 Word32" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int32 Word64" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int32 Word" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Word in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int32 Natural" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Natural.Natural in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int32 Float" ~:
      let f = hush . Witch.tryFrom @Int.Int32 @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      ]
    , "From Int32 Double" ~:
      let f = Witch.from @Int.Int32 @Double in
      [ f 0 ~?= 0
      , f 2147483647 ~?= 2147483647
      , f (-2147483648) ~?= (-2147483648)
      ]

    -- Int64

    , "TryFrom Int64 Int8" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      , f (-128) ~?= Just (-128)
      , f (-129) ~?= Nothing
      ]
    , "TryFrom Int64 Int16" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      , f (-32768) ~?= Just (-32768)
      , f (-32769) ~?= Nothing
      ]
    , "TryFrom Int64 Int32" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f 2147483648 ~?= Nothing
      , f (-2147483648) ~?= Just (-2147483648)
      , f (-2147483649) ~?= Nothing
      ]
    , "TryFrom Int64 Int" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Int in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in if toInteger x >= 9223372036854775807
        then f 9223372036854775807 ~?= Just 9223372036854775807
        else f (fromIntegral x) ~?= Just x
      , let x = minBound :: Int in if toInteger x <= (-9223372036854775808)
        then f (-9223372036854775808) ~?= Just (-9223372036854775808)
        else f (fromIntegral x) ~?= Just x
      ]
    , "From Int64 Integer" ~:
      let f = Witch.from @Int.Int64 @Integer in
      [ f 0 ~?= 0
      , f 9223372036854775807 ~?= 9223372036854775807
      , f (-9223372036854775808) ~?= (-9223372036854775808)
      ]
    , "TryFrom Int64 Word8" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int64 Word16" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int64 Word32" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int64 Word64" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 9223372036854775807 ~?= Just 9223372036854775807
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int64 Word" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Word in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Word in if toInteger x >= 9223372036854775807
        then f 9223372036854775807 ~?= Just 9223372036854775807
        else f (fromIntegral x) ~?= Just x
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int64 Natural" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Natural.Natural in
      [ f 0 ~?= Just 0
      , f 9223372036854775807 ~?= Just 9223372036854775807
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int64 Float" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      ]
    , "TryFrom Int64 Double" ~:
      let f = hush . Witch.tryFrom @Int.Int64 @Double in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      , f (-9007199254740991) ~?= Just (-9007199254740991)
      , f (-9007199254740992) ~?= Nothing
      ]

    -- Int

    , "TryFrom Int Int8" ~:
      let f = hush . Witch.tryFrom @Int @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      , f (-128) ~?= Just (-128)
      , f (-129) ~?= Nothing
      ]
    , "TryFrom Int Int16" ~:
      let f = hush . Witch.tryFrom @Int @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      , f (-32768) ~?= Just (-32768)
      , f (-32769) ~?= Nothing
      ]
    , "TryFrom Int Int32" ~:
      let f = hush . Witch.tryFrom @Int @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , let x = maxBound :: Int in if toInteger x >= 2147483648
        then f 2147483648 ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      , f (-2147483648) ~?= Just (-2147483648)
      , let x = minBound :: Int in if toInteger x <= (-2147483649)
        then f (-2147483649) ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      ]
    , "From Int Int64" ~:
      let f = Witch.from @Int @Int.Int64 in
      [ f 0 ~?= 0
      , f maxBound ~?= fromIntegral (maxBound :: Int)
      , f minBound ~?= fromIntegral (minBound :: Int)
      ]
    , "From Int Integer" ~:
      let f = Witch.from @Int @Integer in
      [ f 0 ~?= 0
      , f maxBound ~?= fromIntegral (maxBound :: Int)
      , f minBound ~?= fromIntegral (minBound :: Int)
      ]
    , "TryFrom Int Word8" ~:
      let f = hush . Witch.tryFrom @Int @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int Word16" ~:
      let f = hush . Witch.tryFrom @Int @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int Word32" ~:
      let f = hush . Witch.tryFrom @Int @Word.Word32 in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in if toInteger x >= 4294967295
        then f 4294967295 ~?= Just 4294967295
        else f x ~?= Just (fromIntegral x)
      , let x = maxBound :: Int in if toInteger x >= 4294967296
        then f 4294967296 ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int Word64" ~:
      let f = hush . Witch.tryFrom @Int @Word.Word64 in
      [ f 0 ~?= Just 0
      , f maxBound ~?= Just (fromIntegral (maxBound :: Int))
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int Word" ~:
      let f = hush . Witch.tryFrom @Int @Word in
      [ f 0 ~?= Just 0
      , f maxBound ~?= Just (fromIntegral (maxBound :: Int))
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int Natural" ~:
      let f = hush . Witch.tryFrom @Int @Natural.Natural in
      [ f 0 ~?= Just 0
      , f maxBound ~?= Just (fromIntegral (maxBound :: Int))
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Int Float" ~:
      let f = hush . Witch.tryFrom @Int @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      ]
    , "TryFrom Int Double" ~:
      let f = hush . Witch.tryFrom @Int @Double in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in if toInteger x >= 9007199254740991
        then f 9007199254740991 ~?= Just 9007199254740991
        else f x ~?= Just (fromIntegral x)
      , let x = maxBound :: Int in if toInteger x >= 9007199254740992
        then f 9007199254740992 ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      , let x = minBound :: Int in if toInteger x <= (-9007199254740991)
        then f (-9007199254740991) ~?= Just (-9007199254740991)
        else f x ~?= Just (fromIntegral x)
      , let x = minBound :: Int in if toInteger x <= (-9007199254740992)
        then f (-9007199254740992) ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      ]

    -- Integer

    , "TryFrom Integer Int8" ~:
      let f = hush . Witch.tryFrom @Integer @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      , f (-128) ~?= Just (-128)
      , f (-129) ~?= Nothing
      ]
    , "TryFrom Integer Int16" ~:
      let f = hush . Witch.tryFrom @Integer @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      , f (-32768) ~?= Just (-32768)
      , f (-32769) ~?= Nothing
      ]
    , "TryFrom Integer Int32" ~:
      let f = hush . Witch.tryFrom @Integer @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f 2147483648 ~?= Nothing
      , f (-2147483648) ~?= Just (-2147483648)
      , f (-2147483649) ~?= Nothing
      ]
    , "TryFrom Integer Int64" ~:
      let f = hush . Witch.tryFrom @Integer @Int.Int64 in
      [ f 0 ~?= Just 0
      , f 9223372036854775807 ~?= Just 9223372036854775807
      , f 9223372036854775808 ~?= Nothing
      , f (-9223372036854775808) ~?= Just (-9223372036854775808)
      , f (-9223372036854775809) ~?= Nothing
      ]
    , "TryFrom Integer Int" ~:
      let f = hush . Witch.tryFrom @Integer @Int in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in f (fromIntegral x) ~?= Just x
      , let x = toInteger (maxBound :: Int) + 1 in f x ~?= Nothing
      , let x = minBound :: Int in f (fromIntegral x) ~?= Just x
      , let x = toInteger (minBound :: Int) - 1 in f x ~?= Nothing
      ]
    , "TryFrom Integer Word8" ~:
      let f = hush . Witch.tryFrom @Integer @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Integer Word16" ~:
      let f = hush . Witch.tryFrom @Integer @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Integer Word32" ~:
      let f = hush . Witch.tryFrom @Integer @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 4294967295 ~?= Just 4294967295
      , f 4294967296 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Integer Word64" ~:
      let f = hush . Witch.tryFrom @Integer @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 18446744073709551615 ~?= Just 18446744073709551615
      , f 18446744073709551616 ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Integer Word" ~:
      let f = hush . Witch.tryFrom @Integer @Word in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Word in f (fromIntegral x) ~?= Just x
      , let x = toInteger (maxBound :: Word) + 1 in f x ~?= Nothing
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Integer Natural" ~:
      let f = hush . Witch.tryFrom @Integer @Natural.Natural in
      [ f 0 ~?= Just 0
      , f 18446744073709551616 ~?= Just 18446744073709551616
      , f (-1) ~?= Nothing
      ]
    , "TryFrom Integer Float" ~:
      let f = hush . Witch.tryFrom @Integer @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      ]
    , "TryFrom Integer Double" ~:
      let f = hush . Witch.tryFrom @Integer @Double in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      , f (-9007199254740991) ~?= Just (-9007199254740991)
      , f (-9007199254740992) ~?= Nothing
      ]

    -- Word8

    , "From Word8 Word16" ~:
      let f = Witch.from @Word.Word8 @Word.Word16 in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Word32" ~:
      let f = Witch.from @Word.Word8 @Word.Word32 in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Word64" ~:
      let f = Witch.from @Word.Word8 @Word.Word64 in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Word" ~:
      let f = Witch.from @Word.Word8 @Word in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Natural" ~:
      let f = Witch.from @Word.Word8 @Natural.Natural in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "TryFrom Word8 Int8" ~:
      let f = hush . Witch.tryFrom @Word.Word8 @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      ]
    , "From Word8 Int16" ~:
      let f = Witch.from @Word.Word8 @Int.Int16 in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Int32" ~:
      let f = Witch.from @Word.Word8 @Int.Int32 in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Int64" ~:
      let f = Witch.from @Word.Word8 @Int.Int64 in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Int" ~:
      let f = Witch.from @Word.Word8 @Int in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Integer" ~:
      let f = Witch.from @Word.Word8 @Integer in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Float" ~:
      let f = Witch.from @Word.Word8 @Float in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]
    , "From Word8 Double" ~:
      let f = Witch.from @Word.Word8 @Double in
      [ f 0 ~?= 0
      , f 255 ~?= 255
      ]

    -- Word16

    , "TryFrom Word16 Word8" ~:
      let f = hush . Witch.tryFrom @Word.Word16 @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      ]
    , "From Word16 Word32" ~:
      let f = Witch.from @Word.Word16 @Word.Word32 in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Word64" ~:
      let f = Witch.from @Word.Word16 @Word.Word64 in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Word" ~:
      let f = Witch.from @Word.Word16 @Word in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Natural" ~:
      let f = Witch.from @Word.Word16 @Natural.Natural in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "TryFrom Word16 Int8" ~:
      let f = hush . Witch.tryFrom @Word.Word16 @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      ]
    , "TryFrom Word16 Int16" ~:
      let f = hush . Witch.tryFrom @Word.Word16 @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      ]
    , "From Word16 Int32" ~:
      let f = Witch.from @Word.Word16 @Int.Int32 in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Int64" ~:
      let f = Witch.from @Word.Word16 @Int.Int64 in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Int" ~:
      let f = Witch.from @Word.Word16 @Int in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Integer" ~:
      let f = Witch.from @Word.Word16 @Integer in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Float" ~:
      let f = Witch.from @Word.Word16 @Float in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]
    , "From Word16 Double" ~:
      let f = Witch.from @Word.Word16 @Double in
      [ f 0 ~?= 0
      , f 65535 ~?= 65535
      ]

    -- Word32

    , "TryFrom Word32 Word8" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      ]
    , "TryFrom Word32 Word16" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      ]
    , "From Word32 Word64" ~:
      let f = Witch.from @Word.Word32 @Word.Word64 in
      [ f 0 ~?= 0
      , f 4294967295 ~?= 4294967295
      ]
    , "TryFrom Word32 Word" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Word in
      [ f 0 ~?= Just 0
      , f 4294967295 ~?= Just 4294967295
      ]
    , "From Word32 Natural" ~:
      let f = Witch.from @Word.Word32 @Natural.Natural in
      [ f 0 ~?= 0
      , f 4294967295 ~?= 4294967295
      ]
    , "TryFrom Word32 Int8" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      ]
    , "TryFrom Word32 Int16" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      ]
    , "TryFrom Word32 Int32" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f 2147483648 ~?= Nothing
      ]
    , "From Word32 Int64" ~:
      let f = Witch.from @Word.Word32 @Int.Int64 in
      [ f 0 ~?= 0
      , f 4294967295 ~?= 4294967295
      ]
    , "TryFrom Word32 Int" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Int in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in if toInteger x >= 4294967295
        then f 4294967295 ~?= Just 4294967295
        else f (fromIntegral x) ~?= Just x
      ]
    , "From Word32 Integer" ~:
      let f = Witch.from @Word.Word32 @Integer in
      [ f 0 ~?= 0
      , f 4294967295 ~?= 4294967295
      ]
    , "TryFrom Word32 Float" ~:
      let f = hush . Witch.tryFrom @Word.Word32 @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      ]
    , "From Word32 Double" ~:
      let f = Witch.from @Word.Word32 @Double in
      [ f 0 ~?= 0
      , f 4294967295 ~?= 4294967295
      ]

    -- Word64

    , "TryFrom Word64 Word8" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      ]
    , "TryFrom Word64 Word16" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      ]
    , "TryFrom Word64 Word32" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 4294967295 ~?= Just 4294967295
      , f 4294967296 ~?= Nothing
      ]
    , "TryFrom Word64 Word" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Word in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Word in if toInteger x >= 18446744073709551615
        then f 18446744073709551615 ~?= Just 18446744073709551615
        else f (fromIntegral x) ~?= Just x
      ]
    , "From Word64 Natural" ~:
      let f = Witch.from @Word.Word64 @Natural.Natural in
      [ f 0 ~?= 0
      , f 18446744073709551615 ~?= 18446744073709551615
      ]
    , "TryFrom Word64 Int8" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      ]
    , "TryFrom Word64 Int16" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      ]
    , "TryFrom Word64 Int32" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f 2147483648 ~?= Nothing
      ]
    , "TryFrom Word64 Int64" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Int.Int64 in
      [ f 0 ~?= Just 0
      , f 9223372036854775807 ~?= Just 9223372036854775807
      , f 9223372036854775808 ~?= Nothing
      ]
    , "TryFrom Word64 Int" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Int in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in hush (Witch.tryFrom @Word.Word64 @Int (fromIntegral x)) ~?= Just x
      , let x = fromIntegral (maxBound :: Int) + 1 :: Word.Word64 in hush (Witch.tryFrom @Word.Word64 @Int x) ~?= Nothing
      ]
    , "From Word64 Integer" ~:
      let f = Witch.from @Word.Word64 @Integer in
      [ f 0 ~?= 0
      , f 18446744073709551615 ~?= 18446744073709551615
      ]
    , "TryFrom Word64 Float" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      ]
    , "TryFrom Word64 Double" ~:
      let f = hush . Witch.tryFrom @Word.Word64 @Double in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      ]

    -- Word

    , "TryFrom Word Word8" ~:
      let f = hush . Witch.tryFrom @Word @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      ]
    , "TryFrom Word Word16" ~:
      let f = hush . Witch.tryFrom @Word @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      ]
    , "TryFrom Word Word32" ~:
      let f = hush . Witch.tryFrom @Word @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 4294967295 ~?= Just 4294967295
      , let x = maxBound :: Word in if toInteger x >= 4294967296
        then f 4294967296 ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      ]
    , "From Word Word64" ~:
      let f = Witch.from @Word @Word.Word64 in
      [ f 0 ~?= 0
      , f maxBound ~?= fromIntegral (maxBound :: Word)
      ]
    , "From Word Natural" ~:
      let f = Witch.from @Word @Natural.Natural in
      [ f 0 ~?= 0
      , f maxBound ~?= fromIntegral (maxBound :: Word)
      ]
    , "TryFrom Word Int8" ~:
      let f = hush . Witch.tryFrom @Word @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      ]
    , "TryFrom Word Int16" ~:
      let f = hush . Witch.tryFrom @Word @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      ]
    , "TryFrom Word Int32" ~:
      let f = hush . Witch.tryFrom @Word @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f 2147483648 ~?= Nothing
      ]
    , "TryFrom Word Int64" ~:
      let f = hush . Witch.tryFrom @Word @Int.Int64 in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Word in if toInteger x >= 9223372036854775807
        then f 9223372036854775807 ~?= Just 9223372036854775807
        else f x ~?= Just (fromIntegral x)
      , let x = maxBound :: Word in if toInteger x >= 9223372036854775808
        then f 9223372036854775808 ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      ]
    , "TryFrom Word Int" ~:
      let f = hush . Witch.tryFrom @Word @Int in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in hush (Witch.tryFrom @Word @Int (fromIntegral x)) ~?= Just x
      , let x = fromIntegral (maxBound :: Int) + 1 :: Word in hush (Witch.tryFrom @Word @Int x) ~?= Nothing
      ]
    , "From Word Integer" ~:
      let f = Witch.from @Word @Integer in
      [ f 0 ~?= 0
      , f maxBound ~?= fromIntegral (maxBound :: Word)
      ]
    , "TryFrom Word Float" ~:
      let f = hush . Witch.tryFrom @Word @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      ]
    , "TryFrom Word Double" ~:
      let f = hush . Witch.tryFrom @Word @Double in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Word in if toInteger x >= 9007199254740991
        then f 9007199254740991 ~?= Just 9007199254740991
        else f x ~?= Just (fromIntegral x)
      , let x = maxBound :: Word in if toInteger x >= 9007199254740992
        then f 9007199254740992 ~?= Nothing
        else f x ~?= Just (fromIntegral x)
      ]

    -- Natural

    , "TryFrom Natural Word8" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      ]
    , "TryFrom Natural Word16" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      ]
    , "TryFrom Natural Word32" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 4294967295 ~?= Just 4294967295
      , f 4294967296 ~?= Nothing
      ]
    , "TryFrom Natural Word64" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 18446744073709551615 ~?= Just 18446744073709551615
      , f 18446744073709551616 ~?= Nothing
      ]
    , "TryFrom Natural Word" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Word in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Word in hush (Witch.tryFrom @Natural.Natural @Word (fromIntegral x)) ~?= Just x
      , let x = fromIntegral (maxBound :: Word) + 1 :: Natural.Natural in hush (Witch.tryFrom @Natural.Natural @Word x) ~?= Nothing
      ]
    , "TryFrom Natural Int8" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      ]
    , "TryFrom Natural Int16" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      ]
    , "TryFrom Natural Int32" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f 2147483648 ~?= Nothing
      ]
    , "TryFrom Natural Int64" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Int.Int64 in
      [ f 0 ~?= Just 0
      , f 9223372036854775807 ~?= Just 9223372036854775807
      , f 9223372036854775808 ~?= Nothing
      ]
    , "TryFrom Natural Int" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Int in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in hush (Witch.tryFrom @Natural.Natural @Int (fromIntegral x)) ~?= Just x
      , let x = fromIntegral (maxBound :: Int) + 1 :: Natural.Natural in hush (Witch.tryFrom @Natural.Natural @Int x) ~?= Nothing
      ]
    , "From Natural Integer" ~:
      let f = Witch.from @Natural.Natural @Integer in
      [ f 0 ~?= 0
      , f 9223372036854775808 ~?= 9223372036854775808
      ]
    , "TryFrom Natural Float" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Float in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      ]
    , "TryFrom Natural Double" ~:
      let f = hush . Witch.tryFrom @Natural.Natural @Double in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      ]

    -- Float

    , "TryFrom Float Int8" ~:
      let f = hush . Witch.tryFrom @Float @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      , f (-128) ~?= Just (-128)
      , f (-129) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Int16" ~:
      let f = hush . Witch.tryFrom @Float @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      , f (-32768) ~?= Just (-32768)
      , f (-32769) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Int32" ~:
      let f = hush . Witch.tryFrom @Float @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Int64" ~:
      let f = hush . Witch.tryFrom @Float @Int.Int64 in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Int" ~:
      let f = hush . Witch.tryFrom @Float @Int in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Integer" ~:
      let f = hush . Witch.tryFrom @Float @Integer in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (-16777215) ~?= Just (-16777215)
      , f (-16777216) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Word8" ~:
      let f = hush . Witch.tryFrom @Float @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Word16" ~:
      let f = hush . Witch.tryFrom @Float @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Word32" ~:
      let f = hush . Witch.tryFrom @Float @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Word64" ~:
      let f = hush . Witch.tryFrom @Float @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Word" ~:
      let f = hush . Witch.tryFrom @Float @Word in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Natural" ~:
      let f = hush . Witch.tryFrom @Float @Natural.Natural in
      [ f 0 ~?= Just 0
      , f 16777215 ~?= Just 16777215
      , f 16777216 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Float Rational" ~:
      let f = hush . Witch.tryFrom @Float @Rational in
      [ f 0 ~?= Just 0
      , f (-0) ~?= Just 0
      , f 0.5 ~?= Just 0.5
      , f (-0.5) ~?= Just (-0.5)
      , f 16777215 ~?= Just 16777215
      , f (-16777215) ~?= Just (-16777215)
      , f 16777216 ~?= Just 16777216
      , f (-16777216) ~?= Just (-16777216)
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      , f 0.1 ~?= Just 0.1
      , f (-0.1) ~?= Just (-0.1)
      ]
    , "From Float Double" ~:
      let f = Witch.from @Float @Double in
      [ f 0 ~?= 0
      , f 0.5 ~?= 0.5
      , f (-0.5) ~?= (-0.5)
      , TestCase $ let x = f (0 / 0) in assertBool (show x) $ isNaN x
      , f (1 / 0) ~?= (1 / 0)
      , f (-1 / 0) ~?= (-1 / 0)
      ]

    -- Double

    , "TryFrom Double Int8" ~:
      let f = hush . Witch.tryFrom @Double @Int.Int8 in
      [ f 0 ~?= Just 0
      , f 127 ~?= Just 127
      , f 128 ~?= Nothing
      , f (-128) ~?= Just (-128)
      , f (-129) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Int16" ~:
      let f = hush . Witch.tryFrom @Double @Int.Int16 in
      [ f 0 ~?= Just 0
      , f 32767 ~?= Just 32767
      , f 32768 ~?= Nothing
      , f (-32768) ~?= Just (-32768)
      , f (-32769) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Int32" ~:
      let f = hush . Witch.tryFrom @Double @Int.Int32 in
      [ f 0 ~?= Just 0
      , f 2147483647 ~?= Just 2147483647
      , f 2147483648 ~?= Nothing
      , f (-2147483648) ~?= Just (-2147483648)
      , f (-2147483649) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Int64" ~:
      let f = hush . Witch.tryFrom @Double @Int.Int64 in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      , f (-9007199254740991) ~?= Just (-9007199254740991)
      , f (-9007199254740992) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Int" ~:
      let f = hush . Witch.tryFrom @Double @Int in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Int in if toInteger x >= 9007199254740991
        then f 9007199254740991 ~?= Just 9007199254740991
        else f (fromIntegral x) ~?= Just x
      , f 9007199254740992 ~?= Nothing
      , let x = minBound :: Int in if toInteger x <= (-9007199254740991)
        then f (-9007199254740991) ~?= Just (-9007199254740991)
        else f (fromIntegral x) ~?= Just x
      , f (-9007199254740992) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Integer" ~:
      let f = hush . Witch.tryFrom @Double @Integer in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      , f (-9007199254740991) ~?= Just (-9007199254740991)
      , f (-9007199254740992) ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Word8" ~:
      let f = hush . Witch.tryFrom @Double @Word.Word8 in
      [ f 0 ~?= Just 0
      , f 255 ~?= Just 255
      , f 256 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Word16" ~:
      let f = hush . Witch.tryFrom @Double @Word.Word16 in
      [ f 0 ~?= Just 0
      , f 65535 ~?= Just 65535
      , f 65536 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Word32" ~:
      let f = hush . Witch.tryFrom @Double @Word.Word32 in
      [ f 0 ~?= Just 0
      , f 4294967295 ~?= Just 4294967295
      , f 4294967296 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Word64" ~:
      let f = hush . Witch.tryFrom @Double @Word.Word64 in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Word" ~:
      let f = hush . Witch.tryFrom @Double @Word in
      [ f 0 ~?= Just 0
      , let x = maxBound :: Word in if toInteger x >= 9007199254740991
        then f 9007199254740991 ~?= Just 9007199254740991
        else f (fromIntegral x) ~?= Just x
      , f 9007199254740992 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Natural" ~:
      let f = hush . Witch.tryFrom @Double @Natural.Natural in
      [ f 0 ~?= Just 0
      , f 9007199254740991 ~?= Just 9007199254740991
      , f 9007199254740992 ~?= Nothing
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      ]
    , "TryFrom Double Rational" ~:
      let f = hush . Witch.tryFrom @Double @Rational in
      [ f 0 ~?= Just 0
      , f (-0) ~?= Just 0
      , f 0.5 ~?= Just 0.5
      , f (-0.5) ~?= Just (-0.5)
      , f 9007199254740991 ~?= Just 9007199254740991
      , f (-9007199254740991) ~?= Just (-9007199254740991)
      , f 9007199254740992 ~?= Just 9007199254740992
      , f (-9007199254740992) ~?= Just (-9007199254740992)
      , f (0 / 0) ~?= Nothing
      , f (1 / 0) ~?= Nothing
      , f (-1 / 0) ~?= Nothing
      , f 0.1 ~?= Just 0.1
      , f (-0.1) ~?= Just (-0.1)
      ]
    , "From Double Float" ~:
      let f = Witch.from @Double @Float in
      [ f 0 ~?= 0
      , f 0.5 ~?= 0.5
      , f (-0.5) ~?= (-0.5)
      , TestCase $ let x = f (0 / 0) in assertBool (show x) $ isNaN x
      , f (1 / 0) ~?= (1 / 0)
      , f (-1 / 0) ~?= (-1 / 0)
      ]

    -- Ratio

    , "From a (Ratio a)" ~:
      let f = Witch.from @Int @(Ratio.Ratio Int) in
      [ Witch.from @Integer @Rational 0 ~?= 0
      , f 0 ~?= 0
      ]
    , "TryFrom (Ratio a) a" ~:
      let f = hush . Witch.tryFrom @(Ratio.Ratio Int) @Int in
      [ hush (Witch.tryFrom @Rational @Integer 0) ~?= Just 0
      , hush (Witch.tryFrom @Rational @Integer 0.5) ~?= Nothing
      , f 0 ~?= Just 0
      , f 0.5 ~?= Nothing
      ]
    , "From Rational Float" ~:
      let f = Witch.from @Rational @Float in
      [ f 0 ~?= 0
      , f 0.5 ~?= 0.5
      , f (-0.5) ~?= (-0.5)
      , f 0.1 ~?= 0.1
      , f (-0.1) ~?= (-0.1)
      ]
    , "From Rational Double" ~:
      let f = Witch.from @Rational @Double in
      [ f 0 ~?= 0
      , f 0.5 ~?= 0.5
      , f (-0.5) ~?= (-0.5)
      , f 0.1 ~?= 0.1
      , f (-0.1) ~?= (-0.1)
      ]
    , "TryFrom Rational (Fixed a)" ~:
      let f = hush . Witch.tryFrom @Rational @Fixed.Deci in
      [ hush (Witch.tryFrom @Rational @Fixed.Uni 1) ~?= Just 1
      , hush (Witch.tryFrom @Rational @Fixed.Uni 1.2) ~?= Nothing
      , f 0.1 ~?= Just 0.1
      , f 1.2 ~?= Just 1.2
      , f 12.3 ~?= Just 12.3
      , f 0.12 ~?= Nothing
      ]

    -- Fixed

    , "From Integer (Fixed a)" ~:
      let f = Witch.from @Integer @Fixed.Deci in
      [ Witch.from @Integer @Fixed.Uni 1 ~?= 1
      , f 1 ~?= 0.1
      , f 10 ~?= 1
      , f 120 ~?= 12
      ]
    , "From (Fixed a) Integer" ~:
      let f = Witch.from @Fixed.Deci @Integer in
      [ Witch.from @Fixed.Uni @Integer 1 ~?= 1
      , f 0.1 ~?= 1
      , f 1 ~?= 10
      , f 12 ~?= 120
      ]
    , "From (Fixed a) Rational" ~:
      let f = Witch.from @Fixed.Deci @Rational in
      [ Witch.from @Fixed.Uni @Rational 1 ~?= 1
      , f 0.1 ~?= 0.1
      , f 1 ~?= 1
      , f 12 ~?= 12
      ]

    -- Complex

    , "From a (Complex a)" ~:
      let f = Witch.from @Float @(Complex.Complex Float) in
      [ Witch.from @Double @(Complex.Complex Double) 1 ~?= 1
      , f 1 ~?= 1
      ]
    , "TryFrom (Complex a) a" ~:
      let f = hush . Witch.tryFrom @(Complex.Complex Float) @Float in
      [ hush (Witch.tryFrom @(Complex.Complex Double) @Double 1) ~?= Just 1
      , hush (Witch.tryFrom @(Complex.Complex Double) @Double (0 Complex.:+ 1)) ~?= Nothing
      , f 1 ~?= Just 1
      , f (0 Complex.:+ 1) ~?= Nothing
      ]

    -- NonEmpty

    , "TryFrom [a] (NonEmpty a)" ~:
      let f = hush . Witch.tryFrom @[Int] @(NonEmpty.NonEmpty Int) in
      [ f [] ~?= Nothing
      , f [1] ~?= Just (1 NonEmpty.:| [])
      , f [1, 2] ~?= Just (1 NonEmpty.:| [2])
      ]
    , "From (NonEmpty a) [a]" ~:
      let f = Witch.from @(NonEmpty.NonEmpty Int) @[Int] in
      [ f (1 NonEmpty.:| []) ~?= [1]
      , f (1 NonEmpty.:| [2]) ~?= [1, 2]
      ]

    -- Set

    , "From [a] (Set a)" ~:
      let f = Witch.from @[Char] @(Set.Set Char) in
      [ f [] ~?= Set.fromList []
      , f ['a'] ~?= Set.fromList ['a']
      , f ['a', 'b'] ~?= Set.fromList ['a', 'b']
      , f ['a', 'a'] ~?= Set.fromList ['a']
      ]
    , "From (Set a) [a]" ~:
      let f = Witch.from @(Set.Set Char) @[Char] in
      [ f (Set.fromList []) ~?= []
      , f (Set.fromList ['a']) ~?= ['a']
      , f (Set.fromList ['a', 'b']) ~?= ['a', 'b']
      ]

    -- IntSet

    , "From [Int] IntSet" ~:
      let f = Witch.from @[Int] @IntSet.IntSet in
      [ f [] ~?= IntSet.fromList []
      , f [1] ~?= IntSet.fromList [1]
      , f [1, 2] ~?= IntSet.fromList [1, 2]
      ]
    , "From IntSet [Int]" ~:
      let f = Witch.from @IntSet.IntSet @[Int] in
      [ f (IntSet.fromList []) ~?= []
      , f (IntSet.fromList [1]) ~?= [1]
      , f (IntSet.fromList [1, 2]) ~?= [1, 2]
      ]

    -- Map

    , "From [(k, v)] (Map k v)" ~:
      let f = Witch.from @[(Char, Int)] @(Map.Map Char Int) in
      [ f [] ~?= Map.empty
      , f [('a', 1)] ~?= Map.fromList [('a', 1)]
      , f [('a', 1), ('b', 2)] ~?= Map.fromList [('a', 1), ('b', 2)]
      , f [('a', 1), ('a', 2)] ~?= Map.fromList [('a', 2)]
      ]
    , "From (Map k v) [(k, v)]" ~:
      let f = Witch.from @(Map.Map Char Int) @[(Char, Int)] in
      [ f Map.empty ~?= []
      , f (Map.fromList [('a', 1)]) ~?= [('a', 1)]
      , f (Map.fromList [('a', 1), ('b', 2)]) ~?= [('a', 1), ('b', 2)]
      ]

    -- IntMap

    , "From [(Int, v)] (IntMap v)" ~:
      let f = Witch.from @[(Int, Char)] @(IntMap.IntMap Char) in
      [ f [] ~?= IntMap.fromList []
      , f [(1, 'a')] ~?= IntMap.fromList [(1, 'a')]
      , f [(1, 'a'), (2, 'b')] ~?= IntMap.fromList [(1, 'a'), (2, 'b')]
      , f [(1, 'a'), (1, 'b')] ~?= IntMap.fromList [(1, 'b')]
      ]
    , "From (IntMap v) [(Int, v)]" ~:
      let f = Witch.from @(IntMap.IntMap Char) @[(Int, Char)] in
      [ f (IntMap.fromList []) ~?= []
      , f (IntMap.fromList [(1, 'a')]) ~?= [(1, 'a')]
      , f (IntMap.fromList [(1, 'a'), (2, 'b')]) ~?= [(1, 'a'), (2, 'b')]
      ]

    -- Seq

    , "From [a] (Seq a)" ~:
      let f = Witch.from @[Int] @(Seq.Seq Int) in
      [ f [] ~?= Seq.fromList []
      , f [1] ~?= Seq.fromList [1]
      , f [1, 2] ~?= Seq.fromList [1, 2]
      ]
    , "From (Seq a) [a]" ~:
      let f = Witch.from @(Seq.Seq Int) @[Int] in
      [ f (Seq.fromList []) ~?= []
      , f (Seq.fromList [1]) ~?= [1]
      , f (Seq.fromList [1, 2]) ~?= [1, 2]
      ]

    -- ByteString

    , "From [Word8] ByteString" ~:
      let f = Witch.from @[Word.Word8] @ByteString.ByteString in
      [ f [] ~?= ByteString.pack []
      , f [0x00] ~?= ByteString.pack [0x00]
      , f [0x0f, 0xf0] ~?= ByteString.pack [0x0f, 0xf0]
      ]
    , "From ByteString [Word8]" ~:
      let f = Witch.from @ByteString.ByteString @[Word.Word8] in
      [ f (ByteString.pack []) ~?= []
      , f (ByteString.pack [0x00]) ~?= [0x00]
      , f (ByteString.pack [0x0f, 0xf0]) ~?= [0x0f, 0xf0]
      ]
    , "From ByteString LazyByteString" ~:
      let f = Witch.from @ByteString.ByteString @LazyByteString.ByteString in
      [ f (ByteString.pack []) ~?= LazyByteString.pack []
      , f (ByteString.pack [0x00]) ~?= LazyByteString.pack [0x00]
      , f (ByteString.pack [0x0f, 0xf0]) ~?= LazyByteString.pack [0x0f, 0xf0]
      ]
    , "From ByteString ShortByteString" ~:
      let f = Witch.from @ByteString.ByteString @ShortByteString.ShortByteString in
      [ f (ByteString.pack []) ~?= ShortByteString.pack []
      , f (ByteString.pack [0x00]) ~?= ShortByteString.pack [0x00]
      , f (ByteString.pack [0x0f, 0xf0]) ~?= ShortByteString.pack [0x0f, 0xf0]
      ]
    , "TryFrom ByteString Text" ~:
      let f = hush . Witch.tryFrom @ByteString.ByteString @Text.Text in
      [ f (ByteString.pack []) ~?= Just (Text.pack "")
      , f (ByteString.pack [0x61]) ~?= Just (Text.pack "a")
      , f (ByteString.pack [0xff]) ~?= Nothing
      ]
    , "TryFrom ByteString LazyText" ~:
      let f = hush . Witch.tryFrom @ByteString.ByteString @LazyText.Text in
      [ f (ByteString.pack []) ~?= Just (LazyText.pack "")
      , f (ByteString.pack [0x61]) ~?= Just (LazyText.pack "a")
      , f (ByteString.pack [0xff]) ~?= Nothing
      ]
    , "TryFrom ByteString String" ~:
      let f = hush . Witch.tryFrom @ByteString.ByteString @String in
      [ f (ByteString.pack []) ~?= Just ""
      , f (ByteString.pack [0x61]) ~?= Just "a"
      , f (ByteString.pack [0xff]) ~?= Nothing
      ]

    -- LazyByteString

    , "From [Word8] LazyByteString" ~:
      let f = Witch.from @[Word.Word8] @LazyByteString.ByteString in
      [ f [] ~?= LazyByteString.pack []
      , f [0x00] ~?= LazyByteString.pack [0x00]
      , f [0x0f, 0xf0] ~?= LazyByteString.pack [0x0f, 0xf0]
      ]
    , "From LazyByteString [Word8]" ~:
      let f = Witch.from @LazyByteString.ByteString @[Word.Word8] in
      [ f (LazyByteString.pack []) ~?= []
      , f (LazyByteString.pack [0x00]) ~?= [0x00]
      , f (LazyByteString.pack [0x0f, 0xf0]) ~?= [0x0f, 0xf0]
      ]
    , "From LazyByteString ByteString" ~:
      let f = Witch.from @LazyByteString.ByteString @ByteString.ByteString in
      [ f (LazyByteString.pack []) ~?= ByteString.pack []
      , f (LazyByteString.pack [0x00]) ~?= ByteString.pack [0x00]
      , f (LazyByteString.pack [0x0f, 0xf0]) ~?= ByteString.pack [0x0f, 0xf0]
      ]
    , "TryFrom LazyByteString LazyText" ~:
      let f = hush . Witch.tryFrom @LazyByteString.ByteString @LazyText.Text in
      [ f (LazyByteString.pack []) ~?= Just (LazyText.pack "")
      , f (LazyByteString.pack [0x61]) ~?= Just (LazyText.pack "a")
      , f (LazyByteString.pack [0xff]) ~?= Nothing
      ]
    , "TryFrom LazyByteString Text" ~:
      let f = hush . Witch.tryFrom @LazyByteString.ByteString @Text.Text in
      [ f (LazyByteString.pack []) ~?= Just (Text.pack "")
      , f (LazyByteString.pack [0x61]) ~?= Just (Text.pack "a")
      , f (LazyByteString.pack [0xff]) ~?= Nothing
      ]
    , "TryFrom LazyByteString String" ~:
      let f = hush . Witch.tryFrom @LazyByteString.ByteString @String in
      [ f (LazyByteString.pack []) ~?= Just ""
      , f (LazyByteString.pack [0x61]) ~?= Just "a"
      , f (LazyByteString.pack [0xff]) ~?= Nothing
      ]

    -- ShortByteString

    , "From [Word8] ShortByteString" ~:
      let f = Witch.from @[Word.Word8] @ShortByteString.ShortByteString in
      [ f [] ~?= ShortByteString.pack []
      , f [0x00] ~?= ShortByteString.pack [0x00]
      , f [0x0f, 0xf0] ~?= ShortByteString.pack [0x0f, 0xf0]
      ]
    , "From ShortByteString [Word8]" ~:
      let f = Witch.from @ShortByteString.ShortByteString @[Word.Word8] in
      [ f (ShortByteString.pack []) ~?= []
      , f (ShortByteString.pack [0x00]) ~?= [0x00]
      , f (ShortByteString.pack [0x0f, 0xf0]) ~?= [0x0f, 0xf0]
      ]
    , "From ShortByteString ByteString" ~:
      let f = Witch.from @ShortByteString.ShortByteString @ByteString.ByteString in
      [ f (ShortByteString.pack []) ~?= ByteString.pack []
      , f (ShortByteString.pack [0x00]) ~?= ByteString.pack [0x00]
      , f (ShortByteString.pack [0x0f, 0xf0]) ~?= ByteString.pack [0x0f, 0xf0]
      ]

    -- Text

    , "From Text LazyText" ~:
      let f = Witch.from @Text.Text @LazyText.Text in
      [ f (Text.pack "") ~?= LazyText.pack ""
      , f (Text.pack "a") ~?= LazyText.pack "a"
      , f (Text.pack "ab") ~?= LazyText.pack "ab"
      ]
    , "From Text ByteString" ~:
      let f = Witch.from @Text.Text @ByteString.ByteString in
      [ f (Text.pack "") ~?= ByteString.pack []
      , f (Text.pack "a") ~?= ByteString.pack [0x61]
      ]
    , "From Text LazyByteString" ~:
      let f = Witch.from @Text.Text @LazyByteString.ByteString in
      [ f (Text.pack "") ~?= LazyByteString.pack []
      , f (Text.pack "a") ~?= LazyByteString.pack [0x61]
      ]

    -- LazyText

    , "From LazyText Text" ~:
      let f = Witch.from @LazyText.Text @Text.Text in
      [ f (LazyText.pack "") ~?= Text.pack ""
      , f (LazyText.pack "a") ~?= Text.pack "a"
      , f (LazyText.pack "ab") ~?= Text.pack "ab"
      ]
    , "From LazyText LazyByteString" ~:
      let f = Witch.from @LazyText.Text @LazyByteString.ByteString in
      [ f (LazyText.pack "") ~?= LazyByteString.pack []
      , f (LazyText.pack "a") ~?= LazyByteString.pack [0x61]
      ]
    , "From LazyText ByteString" ~:
      let f = Witch.from @LazyText.Text @ByteString.ByteString in
      [ f (LazyText.pack "") ~?= ByteString.pack []
      , f (LazyText.pack "a") ~?= ByteString.pack [0x61]
      ]

    -- String

    , "From String Text" ~:
      let f = Witch.from @String @Text.Text in
      [ f "" ~?= Text.pack ""
      , f "a" ~?= Text.pack "a"
      , f "ab" ~?= Text.pack "ab"
      ]
    , "From Text String" ~:
      let f = Witch.from @Text.Text @String in
      [ f (Text.pack "") ~?= ""
      , f (Text.pack "a") ~?= "a"
      , f (Text.pack "ab") ~?= "ab"
      ]
    , "From String LazyText" ~:
      let f = Witch.from @String @LazyText.Text in
      [ f "" ~?= LazyText.pack ""
      , f "a" ~?= LazyText.pack "a"
      , f "ab" ~?= LazyText.pack "ab"
      ]
    , "From LazyText String" ~:
      let f = Witch.from @LazyText.Text @String in
      [ f (LazyText.pack "") ~?= ""
      , f (LazyText.pack "a") ~?= "a"
      , f (LazyText.pack "ab") ~?= "ab"
      ]
    , "From String ByteString" ~:
      let f = Witch.from @String @ByteString.ByteString in
      [ f "" ~?= ByteString.pack []
      , f "a" ~?= ByteString.pack [0x61]
      ]
    , "From String LazyByteString" ~:
      let f = Witch.from @String @LazyByteString.ByteString in
      [ f "" ~?= LazyByteString.pack []
      , f "a" ~?= LazyByteString.pack [0x61]
      ]

    -- Day

    , "From Integer Day" ~:
      let f = Witch.from @Integer @Time.Day in
      [ f 0 ~?= Time.ModifiedJulianDay 0
      ]
    , "From Day Integer" ~:
      let f = Witch.from @Time.Day @Integer in
      [ f (Time.ModifiedJulianDay 0) ~?= 0
      ]

    -- DayOfWeek

    , "From Day DayOfWeek" ~:
      let f = Witch.from @Time.Day @Time.DayOfWeek in
      [ f (Time.ModifiedJulianDay 0) ~?= Time.Wednesday
      ]

    -- UniversalTime

    , "From Rational UniversalTime" ~:
      let f = Witch.from @Rational @Time.UniversalTime in
      [ f 0 ~?= Time.ModJulianDate 0
      ]
    , "From UniversalTime Rational" ~:
      let f = Witch.from @Time.UniversalTime @Rational in
      [ f (Time.ModJulianDate 0) ~?= 0
      ]

    -- DiffTime

    , "From Pico DiffTime" ~:
      let f = Witch.from @Fixed.Pico @Time.DiffTime in
      [ f 0 ~?= 0
      ]
    , "From DiffTime Pico" ~:
      let f = Witch.from @Time.DiffTime @Fixed.Pico in
      [ f 0 ~?= 0
      ]

    -- NominalDiffTime

    , "From Pico NominalDiffTime" ~:
      let f = Witch.from @Fixed.Pico @Time.NominalDiffTime in
      [ f 0 ~?= 0
      ]
    , "From NominalDiffTime Pico" ~:
      let f = Witch.from @Time.NominalDiffTime @Fixed.Pico in
      [ f 0 ~?= 0
      ]

    -- POSIXTime

    , "From SystemTime POSIXTime" ~:
      let f = Witch.from @Time.SystemTime @Time.POSIXTime in
      [ f (Time.MkSystemTime 0 0) ~?= 0
      ]
    , "From UTCTime POSIXTime" ~:
      let f = Witch.from @Time.UTCTime @Time.POSIXTime in
      [ f unixEpoch ~?= 0
      ]
    , "From POSIXTime UTCTime" ~:
      let f = Witch.from @Time.POSIXTime @Time.UTCTime in
      [ f 0 ~?= unixEpoch
      ]

    -- SystemTime

    , "From UTCTime SystemTime" ~:
      let f = Witch.from @Time.UTCTime @Time.SystemTime in
      [ f unixEpoch ~?= Time.MkSystemTime 0 0
      ]
    , "From SystemTime AbsoluteTime" ~:
      let f = Witch.from @Time.SystemTime @Time.AbsoluteTime in
      [ f (Time.MkSystemTime (-3506716800) 0) ~?= Time.taiEpoch
      ]
    , "From SystemTime UTCTime" ~:
      let f = Witch.from @Time.SystemTime @Time.UTCTime in
      [ f (Time.MkSystemTime 0 0) ~?= unixEpoch
      ]

    -- TimeOfDay

    , "From DiffTime TimeOfDay" ~:
      let f = Witch.from @Time.DiffTime @Time.TimeOfDay in
      [ f 0 ~?= Time.TimeOfDay 0 0 0
      ]
    , "From Rational TimeOfDay" ~:
      let f = Witch.from @Rational @Time.TimeOfDay in
      [ f 0 ~?= Time.TimeOfDay 0 0 0
      ]
    , "From TimeOfDay DiffTime" ~:
      let f = Witch.from @Time.TimeOfDay @Time.DiffTime in
      [ f (Time.TimeOfDay 0 0 0) ~?= 0
      ]
    , "From TimeOfDay Rational" ~:
      let f = Witch.from @Time.TimeOfDay @Rational in
      [ f (Time.TimeOfDay 0 0 0) ~?= 0
      ]

    -- CalendarDiffTime

    , "From CalendarDiffDays CalendarDiffTime" ~:
      let f = Witch.from @Time.CalendarDiffDays @Time.CalendarDiffTime in
      [ f (Time.CalendarDiffDays 0 0) ~?= Time.CalendarDiffTime 0 0
      ]
    , "From NominalDiffTime CalendarDiffTime" ~:
      let f = Witch.from @Time.NominalDiffTime @Time.CalendarDiffTime in
      [ f 0 ~?= Time.CalendarDiffTime 0 0
      ]

    -- ZonedTime

    , "From ZonedTime UTCTime" ~:
      let f = Witch.from @Time.ZonedTime @Time.UTCTime in
      [ f (Time.ZonedTime (Time.LocalTime (Time.ModifiedJulianDay 0) (Time.TimeOfDay 0 0 0)) Time.utc) ~?= Time.UTCTime (Time.ModifiedJulianDay 0) 0
      ]
    ]
  ]

unixEpoch :: Time.UTCTime
unixEpoch = Time.UTCTime (Time.ModifiedJulianDay 40587) 0

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

newtype Age
  = Age Int.Int8
  deriving (Eq, Show)

instance Witch.From Age Int.Int8

instance Witch.From Int.Int8 Age
