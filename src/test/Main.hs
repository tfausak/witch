{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Data.Complex
import Data.Either
import Data.Fixed
import Data.Int
import Data.List.NonEmpty
import Data.Ratio
import Data.Word
import Numeric.Natural
import Test.HUnit
import Witch

main :: IO ()
main = runTestTTAndExit $ "Witch" ~: tests

tests :: [Test]
tests =
  [ as @Int8 1 ~?= 1
  , cast (1 :: Int8) ~?= (1 :: Int16)
  , from @Int8 1 ~?= (1 :: Int)
  , into @Int16 (1 :: Int8) ~?= 1
  , over @String (<> "!") (Name "Simon") ~?= Name "Simon!"
  , via @Int16 (1 :: Int8) ~?= (1 :: Int32)
  , tryFrom @Int16 1 ~?= Right (1 :: Int8)
  , tryInto @Int8 (1 :: Int16) ~?= Right 1
  , unsafeCast (1 :: Int16) ~?= (1 :: Int8)
  , unsafeFrom @Int16 1 ~?= (1 :: Int8)
  , unsafeInto @Int8 (1 :: Int16) ~?= 1
  , ($$( liftedCast (1 :: Int16) ) :: Int8) ~?= 1
  , ($$( liftedFrom @Int16 1 ) :: Int8) ~?= 1
  , ($$( liftedInto @Int8 (1 :: Int16) ) :: Int8) ~?= 1

  -- NonEmpty

  , "TryCast [a] (NonEmpty a)" ~:
    [ tryCast @[Int] @(NonEmpty Int) [] ~?& isLeft
    , tryCast @[Int] @(NonEmpty Int) [1] ~?= Right (1 :| [])
    , tryCast @[Int] @(NonEmpty Int) [1, 2] ~?= Right (1 :| [2])
    ]
  , "Cast (NonEmpty a) [a]" ~:
    [ cast @(NonEmpty Int) @[Int] (1 :| []) ~?= [1]
    , cast @(NonEmpty Int) @[Int] (1 :| [2]) ~?= [1, 2]
    ]

  -- Int8

  , "Cast Int8 Int16" ~:
    [ cast @Int8 @Int16 0 ~?= 0
    , cast @Int8 @Int16 127 ~?= 127
    , cast @Int8 @Int16 (-128) ~?= -128
    ]
  , "Cast Int8 Int32" ~:
    [ cast @Int8 @Int32 0 ~?= 0
    , cast @Int8 @Int32 127 ~?= 127
    , cast @Int8 @Int32 (-128) ~?= -128
    ]
  , "Cast Int8 Int64" ~:
    [ cast @Int8 @Int64 0 ~?= 0
    , cast @Int8 @Int64 127 ~?= 127
    , cast @Int8 @Int64 (-128) ~?= -128
    ]
  , "Cast Int8 Int" ~:
    [ cast @Int8 @Int 0 ~?= 0
    , cast @Int8 @Int 127 ~?= 127
    , cast @Int8 @Int (-128) ~?= -128
    ]
  , "Cast Int8 Integer" ~:
    [ cast @Int8 @Integer 0 ~?= 0
    , cast @Int8 @Integer 127 ~?= 127
    , cast @Int8 @Integer (-128) ~?= -128
    ]
  , "TryCast Int8 Word8" ~:
    [ tryCast @Int8 @Word8 0 ~?= Right 0
    , tryCast @Int8 @Word8 127 ~?= Right 127
    , tryCast @Int8 @Word8 (-1) ~?& isLeft
    ]
  , "TryCast Int8 Word16" ~:
    [ tryCast @Int8 @Word16 0 ~?= Right 0
    , tryCast @Int8 @Word16 127 ~?= Right 127
    , tryCast @Int8 @Word16 (-1) ~?& isLeft
    ]
  , "TryCast Int8 Word32" ~:
    [ tryCast @Int8 @Word32 0 ~?= Right 0
    , tryCast @Int8 @Word32 127 ~?= Right 127
    , tryCast @Int8 @Word32 (-1) ~?& isLeft
    ]
  , "TryCast Int8 Word64" ~:
    [ tryCast @Int8 @Word64 0 ~?= Right 0
    , tryCast @Int8 @Word64 127 ~?= Right 127
    , tryCast @Int8 @Word64 (-1) ~?& isLeft
    ]
  , "TryCast Int8 Word" ~:
    [ tryCast @Int8 @Word 0 ~?= Right 0
    , tryCast @Int8 @Word 127 ~?= Right 127
    , tryCast @Int8 @Word (-1) ~?& isLeft
    ]
  , "TryCast Int8 Natural" ~:
    [ tryCast @Int8 @Natural 0 ~?= Right 0
    , tryCast @Int8 @Natural 127 ~?= Right 127
    , tryCast @Int8 @Natural (-1) ~?& isLeft
    ]
  , "Cast Int8 Float" ~:
    [ cast @Int8 @Float 0 ~?= 0
    , cast @Int8 @Float 127 ~?= 127
    , cast @Int8 @Float (-128) ~?= (-128)
    ]
  , "Cast Int8 Double" ~:
    [ cast @Int8 @Double 0 ~?= 0
    , cast @Int8 @Double 127 ~?= 127
    , cast @Int8 @Double (-128) ~?= (-128)
    ]

  -- Int16

  , "TryCast Int16 Int8" ~:
    [ tryCast @Int16 @Int8 0 ~?= Right 0
    , tryCast @Int16 @Int8 127 ~?= Right 127
    , tryCast @Int16 @Int8 128 ~?& isLeft
    , tryCast @Int16 @Int8 (-128) ~?= Right (-128)
    , tryCast @Int16 @Int8 (-129) ~?& isLeft
    ]
  , "Cast Int16 Int32" ~:
    [ cast @Int16 @Int32 0 ~?= 0
    , cast @Int16 @Int32 32767 ~?= 32767
    , cast @Int16 @Int32 (-32768) ~?= -32768
    ]
  , "Cast Int16 Int64" ~:
    [ cast @Int16 @Int64 0 ~?= 0
    , cast @Int16 @Int64 32767 ~?= 32767
    , cast @Int16 @Int64 (-32768) ~?= -32768
    ]
  , "Cast Int16 Int" ~:
    [ cast @Int16 @Int 0 ~?= 0
    , cast @Int16 @Int 32767 ~?= 32767
    , cast @Int16 @Int (-32768) ~?= -32768
    ]
  , "Cast Int16 Integer" ~:
    [ cast @Int16 @Integer 0 ~?= 0
    , cast @Int16 @Integer 32767 ~?= 32767
    , cast @Int16 @Integer (-32768) ~?= -32768
    ]
  , "TryCast Int16 Word8" ~:
    [ tryCast @Int16 @Word8 0 ~?= Right 0
    , tryCast @Int16 @Word8 255 ~?= Right 255
    , tryCast @Int16 @Word8 256 ~?& isLeft
    , tryCast @Int16 @Word8 (-1) ~?& isLeft
    ]
  , "TryCast Int16 Word16" ~:
    [ tryCast @Int16 @Word16 0 ~?= Right 0
    , tryCast @Int16 @Word16 127 ~?= Right 127
    , tryCast @Int16 @Word16 (-1) ~?& isLeft
    ]
  , "TryCast Int16 Word32" ~:
    [ tryCast @Int16 @Word32 0 ~?= Right 0
    , tryCast @Int16 @Word32 32767 ~?= Right 32767
    , tryCast @Int16 @Word32 (-1) ~?& isLeft
    ]
  , "TryCast Int16 Word64" ~:
    [ tryCast @Int16 @Word64 0 ~?= Right 0
    , tryCast @Int16 @Word64 32767 ~?= Right 32767
    , tryCast @Int16 @Word64 (-1) ~?& isLeft
    ]
  , "TryCast Int16 Word" ~:
    [ tryCast @Int16 @Word 0 ~?= Right 0
    , tryCast @Int16 @Word 32767 ~?= Right 32767
    , tryCast @Int16 @Word (-1) ~?& isLeft
    ]
  , "TryCast Int16 Natural" ~:
    [ tryCast @Int16 @Natural 0 ~?= Right 0
    , tryCast @Int16 @Natural 32767 ~?= Right 32767
    , tryCast @Int16 @Natural (-1) ~?& isLeft
    ]
  , "Cast Int16 Float" ~:
    [ cast @Int16 @Float 0 ~?= 0
    , cast @Int16 @Float 32767 ~?= 32767
    , cast @Int16 @Float (-32768) ~?= (-32768)
    ]
  , "Cast Int16 Double" ~:
    [ cast @Int16 @Double 0 ~?= 0
    , cast @Int16 @Double 32767 ~?= 32767
    , cast @Int16 @Double (-32768) ~?= (-32768)
    ]

  -- Int32

  , "TryCast Int32 Int8" ~:
    [ tryCast @Int32 @Int8 0 ~?= Right 0
    , tryCast @Int32 @Int8 127 ~?= Right 127
    , tryCast @Int32 @Int8 128 ~?& isLeft
    , tryCast @Int32 @Int8 (-128) ~?= Right (-128)
    , tryCast @Int32 @Int8 (-129) ~?& isLeft
    ]
  , "TryCast Int32 Int16" ~:
    [ tryCast @Int32 @Int16 0 ~?= Right 0
    , tryCast @Int32 @Int16 32767 ~?= Right 32767
    , tryCast @Int32 @Int16 32768 ~?& isLeft
    , tryCast @Int32 @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Int32 @Int16 (-32769) ~?& isLeft
    ]
  , "Cast Int32 Int64" ~:
    [ cast @Int32 @Int64 0 ~?= 0
    , cast @Int32 @Int64 2147483647 ~?= 2147483647
    , cast @Int32 @Int64 (-2147483648) ~?= -2147483648
    ]
  , "TryCast Int32 Int" ~:
    if toInteger (maxBound :: Int) < 2147483647 then untested else
      [ tryCast @Int32 @Int 0 ~?= Right 0
      , tryCast @Int32 @Int 2147483647 ~?= Right 2147483647
      , tryCast @Int32 @Int (-2147483648) ~?= Right (-2147483648)
      ]
  , "Cast Int32 Integer" ~:
    [ cast @Int32 @Integer 0 ~?= 0
    , cast @Int32 @Integer 2147483647 ~?= 2147483647
    , cast @Int32 @Integer (-2147483648) ~?= -2147483648
    ]
  , "TryCast Int32 Word8" ~:
    [ tryCast @Int32 @Word8 0 ~?= Right 0
    , tryCast @Int32 @Word8 255 ~?= Right 255
    , tryCast @Int32 @Word8 256 ~?& isLeft
    , tryCast @Int32 @Word8 (-1) ~?& isLeft
    ]
  , "TryCast Int32 Word16" ~:
    [ tryCast @Int32 @Word16 0 ~?= Right 0
    , tryCast @Int32 @Word16 65535 ~?= Right 65535
    , tryCast @Int32 @Word16 65536 ~?& isLeft
    , tryCast @Int32 @Word16 (-1) ~?& isLeft
    ]
  , "TryCast Int32 Word32" ~:
    [ tryCast @Int32 @Word32 0 ~?= Right 0
    , tryCast @Int32 @Word32 2147483647 ~?= Right 2147483647
    , tryCast @Int32 @Word32 (-1) ~?& isLeft
    ]
  , "TryCast Int32 Word64" ~:
    [ tryCast @Int32 @Word64 0 ~?= Right 0
    , tryCast @Int32 @Word64 2147483647 ~?= Right 2147483647
    , tryCast @Int32 @Word64 (-1) ~?& isLeft
    ]
  , "TryCast Int32 Word" ~:
    if toInteger (maxBound :: Word) < 2147483647 then untested else
      [ tryCast @Int32 @Word 0 ~?= Right 0
      , tryCast @Int32 @Word 2147483647 ~?= Right 2147483647
      , tryCast @Int32 @Word (-1) ~?& isLeft
      ]
  , "TryCast Int32 Natural" ~:
    [ tryCast @Int32 @Natural 0 ~?= Right 0
    , tryCast @Int32 @Natural 2147483647 ~?= Right 2147483647
    , tryCast @Int32 @Natural (-1) ~?& isLeft
    ]
  , "TryCast Int32 Float" ~:
    [ tryCast @Int32 @Float 0 ~?= Right 0
    , tryCast @Int32 @Float 16777216 ~?= Right 16777216
    , tryCast @Int32 @Float 16777217 ~?& isLeft
    , tryCast @Int32 @Float (-16777216) ~?= Right (-16777216)
    , tryCast @Int32 @Float (-16777217) ~?& isLeft
    ]
  , "Cast Int32 Double" ~:
    [ cast @Int32 @Double 0 ~?= 0
    , cast @Int32 @Double 2147483647 ~?= 2147483647
    , cast @Int32 @Double (-2147483648) ~?= (-2147483648)
    ]

  -- Int64

  , "TryCast Int64 Int8" ~:
    [ tryCast @Int64 @Int8 0 ~?= Right 0
    , tryCast @Int64 @Int8 127 ~?= Right 127
    , tryCast @Int64 @Int8 128 ~?& isLeft
    , tryCast @Int64 @Int8 (-128) ~?= Right (-128)
    , tryCast @Int64 @Int8 (-129) ~?& isLeft
    ]
  , "TryCast Int64 Int16" ~:
    [ tryCast @Int64 @Int16 0 ~?= Right 0
    , tryCast @Int64 @Int16 32767 ~?= Right 32767
    , tryCast @Int64 @Int16 32768 ~?& isLeft
    , tryCast @Int64 @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Int64 @Int16 (-32769) ~?& isLeft
    ]
  , "TryCast Int64 Int32" ~:
    [ tryCast @Int64 @Int32 0 ~?= Right 0
    , tryCast @Int64 @Int32 2147483647 ~?= Right 2147483647
    , tryCast @Int64 @Int32 2147483648 ~?& isLeft
    , tryCast @Int64 @Int32 (-2147483648) ~?= Right (-2147483648)
    , tryCast @Int64 @Int32 (-2147483649) ~?& isLeft
    ]
  , "TryCast Int64 Int" ~:
    if toInteger (maxBound :: Int) < 9223372036854775807 then untested else
      [ tryCast @Int64 @Int 0 ~?= Right 0
      , tryCast @Int64 @Int 9223372036854775807 ~?= Right 9223372036854775807
      , tryCast @Int64 @Int (-9223372036854775808) ~?= Right (-9223372036854775808)
      ]
  , "Cast Int64 Integer" ~:
    [ cast @Int64 @Integer 0 ~?= 0
    , cast @Int64 @Integer 9223372036854775807 ~?= 9223372036854775807
    , cast @Int64 @Integer (-9223372036854775808) ~?= -9223372036854775808
    ]
  , "TryCast Int64 Word8" ~:
    [ tryCast @Int64 @Word8 0 ~?= Right 0
    , tryCast @Int64 @Word8 255 ~?= Right 255
    , tryCast @Int64 @Word8 256 ~?& isLeft
    , tryCast @Int64 @Word8 (-1) ~?& isLeft
    ]
  , "TryCast Int64 Word16" ~:
    [ tryCast @Int64 @Word16 0 ~?= Right 0
    , tryCast @Int64 @Word16 65535 ~?= Right 65535
    , tryCast @Int64 @Word16 65536 ~?& isLeft
    , tryCast @Int64 @Word16 (-1) ~?& isLeft
    ]
  , "TryCast Int64 Word32" ~:
    [ tryCast @Int64 @Word32 0 ~?= Right 0
    , tryCast @Int64 @Word32 2147483647 ~?= Right 2147483647
    , tryCast @Int64 @Word32 (-1) ~?& isLeft
    ]
  , "TryCast Int64 Word64" ~:
    [ tryCast @Int64 @Word64 0 ~?= Right 0
    , tryCast @Int64 @Word64 9223372036854775807 ~?= Right 9223372036854775807
    , tryCast @Int64 @Word64 (-1) ~?& isLeft
    ]
  , "TryCast Int64 Word" ~:
    if toInteger (maxBound :: Word) < 9223372036854775807 then untested else
      [ tryCast @Int64 @Word 0 ~?= Right 0
      , tryCast @Int64 @Word 9223372036854775807 ~?= Right 9223372036854775807
      , tryCast @Int64 @Word (-1) ~?& isLeft
      ]
  , "TryCast Int64 Natural" ~:
    [ tryCast @Int64 @Natural 0 ~?= Right 0
    , tryCast @Int64 @Natural 9223372036854775807 ~?= Right 9223372036854775807
    , tryCast @Int64 @Natural (-1) ~?& isLeft
    ]
  , "TryCast Int64 Float" ~:
    [ tryCast @Int64 @Float 0 ~?= Right 0
    , tryCast @Int64 @Float 16777216 ~?= Right 16777216
    , tryCast @Int64 @Float 16777217 ~?& isLeft
    , tryCast @Int64 @Float (-16777216) ~?= Right (-16777216)
    , tryCast @Int64 @Float (-16777217) ~?& isLeft
    ]
  , "TryCast Int64 Double" ~:
    [ tryCast @Int64 @Double 0 ~?= Right 0
    , tryCast @Int64 @Double 9007199254740992 ~?= Right 9007199254740992
    , tryCast @Int64 @Double 9007199254740993 ~?& isLeft
    , tryCast @Int64 @Double (-9007199254740992) ~?= Right (-9007199254740992)
    , tryCast @Int64 @Double (-9007199254740993) ~?& isLeft
    ]

  -- Int

  , "TryCast Int Int8" ~:
    [ tryCast @Int @Int8 0 ~?= Right 0
    , tryCast @Int @Int8 127 ~?= Right 127
    , tryCast @Int @Int8 128 ~?& isLeft
    , tryCast @Int @Int8 (-128) ~?= Right (-128)
    , tryCast @Int @Int8 (-129) ~?& isLeft
    ]
  , "TryCast Int Int16" ~:
    [ tryCast @Int @Int16 0 ~?= Right 0
    , tryCast @Int @Int16 32767 ~?= Right 32767
    , tryCast @Int @Int16 32768 ~?& isLeft
    , tryCast @Int @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Int @Int16 (-32769) ~?& isLeft
    ]
  , "TryCast Int Int32" ~:
    if toInteger (maxBound :: Int) < 2147483647 then untested else
      [ tryCast @Int @Int32 0 ~?= Right 0
      , tryCast @Int @Int32 2147483647 ~?= Right 2147483647
      , tryCast @Int @Int32 2147483648 ~?& isLeft
      , tryCast @Int @Int32 (-2147483648) ~?= Right (-2147483648)
      , tryCast @Int @Int32 (-2147483649) ~?& isLeft
      ]
  , "Cast Int Int64" ~:
    [ cast @Int @Int64 0 ~?= 0
    , cast @Int @Int64 maxBound ~?= fromIntegral (maxBound :: Int)
    , cast @Int @Int64 minBound ~?= fromIntegral (minBound :: Int)
    ]
  , "Cast Int Integer" ~:
    [ cast @Int @Integer 0 ~?= 0
    , cast @Int @Integer maxBound ~?= fromIntegral (maxBound :: Int)
    , cast @Int @Integer minBound ~?= fromIntegral (minBound :: Int)
    ]
  , "TryCast Int Word8" ~:
    [ tryCast @Int @Word8 0 ~?= Right 0
    , tryCast @Int @Word8 255 ~?= Right 255
    , tryCast @Int @Word8 256 ~?& isLeft
    , tryCast @Int @Word8 (-1) ~?& isLeft
    ]
  , "TryCast Int Word16" ~:
    [ tryCast @Int @Word16 0 ~?= Right 0
    , tryCast @Int @Word16 65535 ~?= Right 65535
    , tryCast @Int @Word16 65536 ~?& isLeft
    , tryCast @Int @Word16 (-1) ~?& isLeft
    ]
  , "TryCast Int Word32" ~:
    if toInteger (maxBound :: Int) < 4294967295 then untested else
      [ tryCast @Int @Word32 0 ~?= Right 0
      , tryCast @Int @Word32 4294967295 ~?= Right 4294967295
      , tryCast @Int @Word32 4294967296 ~?& isLeft
      , tryCast @Int @Word32 (-1) ~?& isLeft
      ]
  , "TryCast Int Word64" ~:
    [ tryCast @Int @Word64 0 ~?= Right 0
    , tryCast @Int @Word64 maxBound ~?= Right (fromIntegral (maxBound :: Int))
    , tryCast @Int @Word64 (-1) ~?& isLeft
    ]
  , "TryCast Int Word" ~:
    [ tryCast @Int @Word 0 ~?= Right 0
    , tryCast @Int @Word maxBound ~?= Right (fromIntegral (maxBound :: Int))
    , tryCast @Int @Word (-1) ~?& isLeft
    ]
  , "TryCast Int Natural" ~:
    [ tryCast @Int @Natural 0 ~?= Right 0
    , tryCast @Int @Natural maxBound ~?= Right (fromIntegral (maxBound :: Int))
    , tryCast @Int @Natural (-1) ~?& isLeft
    ]
  , "TryCast Int Float" ~:
    [ tryCast @Int @Float 0 ~?= Right 0
    , tryCast @Int @Float 16777216 ~?= Right 16777216
    , tryCast @Int @Float 16777217 ~?& isLeft
    , tryCast @Int @Float (-16777216) ~?= Right (-16777216)
    , tryCast @Int @Float (-16777217) ~?& isLeft
    ]
  , "TryCast Int Double" ~:
    if toInteger (maxBound :: Int) <= 9007199254740992 then untested else
    [ tryCast @Int @Double 0 ~?= Right 0
    , tryCast @Int @Double 9007199254740992 ~?= Right 9007199254740992
    , tryCast @Int @Double 9007199254740993 ~?& isLeft
    , tryCast @Int @Double (-9007199254740992) ~?= Right (-9007199254740992)
    , tryCast @Int @Double (-9007199254740993) ~?& isLeft
    ]

  -- Integer

  , "TryCast Integer Int8" ~:
    [ tryCast @Integer @Int8 0 ~?= Right 0
    , tryCast @Integer @Int8 127 ~?= Right 127
    , tryCast @Integer @Int8 128 ~?& isLeft
    , tryCast @Integer @Int8 (-128) ~?= Right (-128)
    , tryCast @Integer @Int8 (-129) ~?& isLeft
    ]
  , "TryCast Integer Int16" ~:
    [ tryCast @Integer @Int16 0 ~?= Right 0
    , tryCast @Integer @Int16 32767 ~?= Right 32767
    , tryCast @Integer @Int16 32768 ~?& isLeft
    , tryCast @Integer @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Integer @Int16 (-32769) ~?& isLeft
    ]
  , "TryCast Integer Int32" ~:
    [ tryCast @Integer @Int32 0 ~?= Right 0
    , tryCast @Integer @Int32 2147483647 ~?= Right 2147483647
    , tryCast @Integer @Int32 2147483648 ~?& isLeft
    , tryCast @Integer @Int32 (-2147483648) ~?= Right (-2147483648)
    , tryCast @Integer @Int32 (-2147483649) ~?& isLeft
    ]
  , "TryCast Integer Int64" ~:
    [ tryCast @Integer @Int64 0 ~?= Right 0
    , tryCast @Integer @Int64 9223372036854775807 ~?= Right 9223372036854775807
    , tryCast @Integer @Int64 9223372036854775808 ~?& isLeft
    , tryCast @Integer @Int64 (-9223372036854775808) ~?= Right (-9223372036854775808)
    , tryCast @Integer @Int64 (-9223372036854775809) ~?& isLeft
    ]
  , "TryCast Integer Int" ~:
    [ tryCast @Integer @Int 0 ~?= Right 0
    , let x = maxBound :: Int
      in tryCast @Integer @Int (fromIntegral x) ~?= Right x
    , let x = toInteger (maxBound :: Int) + 1
      in tryCast @Integer @Int x ~?& isLeft
    , let x = minBound :: Int
      in tryCast @Integer @Int (fromIntegral x) ~?= Right x
    , let x = toInteger (minBound :: Int) - 1
      in tryCast @Integer @Int x ~?& isLeft
    ]
  , "TryCast Integer Word8" ~:
    [ tryCast @Integer @Word8 0 ~?= Right 0
    , tryCast @Integer @Word8 255 ~?= Right 255
    , tryCast @Integer @Word8 256 ~?& isLeft
    , tryCast @Integer @Word8 (-1) ~?& isLeft
    ]
  , "TryCast Integer Word16" ~:
    [ tryCast @Integer @Word16 0 ~?= Right 0
    , tryCast @Integer @Word16 65535 ~?= Right 65535
    , tryCast @Integer @Word16 65536 ~?& isLeft
    , tryCast @Integer @Word16 (-1) ~?& isLeft
    ]
  , "TryCast Integer Word32" ~:
    [ tryCast @Integer @Word32 0 ~?= Right 0
    , tryCast @Integer @Word32 4294967295 ~?= Right 4294967295
    , tryCast @Integer @Word32 4294967296 ~?& isLeft
    , tryCast @Integer @Word32 (-1) ~?& isLeft
    ]
  , "TryCast Integer Word64" ~:
    [ tryCast @Integer @Word64 0 ~?= Right 0
    , tryCast @Integer @Word64 18446744073709551615 ~?= Right 18446744073709551615
    , tryCast @Integer @Word64 18446744073709551616 ~?& isLeft
    , tryCast @Integer @Word64 (-1) ~?& isLeft
    ]
  , "TryCast Integer Word" ~:
    [ tryCast @Integer @Word 0 ~?= Right 0
    , let x = maxBound :: Word
      in tryCast @Integer @Word (fromIntegral x) ~?= Right x
    , let x = toInteger (maxBound :: Word) + 1
      in tryCast @Integer @Word x ~?& isLeft
    , tryCast @Integer @Word (-1) ~?& isLeft
    ]
  , "TryCast Integer Natural" ~:
    [ tryCast @Integer @Natural 0 ~?= Right 0
    , tryCast @Integer @Natural 18446744073709551616 ~?= Right 18446744073709551616
    , tryCast @Integer @Natural (-1) ~?& isLeft
    ]
  , "TryCast Integer Float" ~:
    [ tryCast @Integer @Float 0 ~?= Right 0
    , tryCast @Integer @Float 16777216 ~?= Right 16777216
    , tryCast @Integer @Float 16777217 ~?& isLeft
    , tryCast @Integer @Float (-16777216) ~?= Right (-16777216)
    , tryCast @Integer @Float (-16777217) ~?& isLeft
    ]
  , "TryCast Integer Double" ~:
    [ tryCast @Integer @Double 0 ~?= Right 0
    , tryCast @Integer @Double 9007199254740992 ~?= Right 9007199254740992
    , tryCast @Integer @Double 9007199254740993 ~?& isLeft
    , tryCast @Integer @Double (-9007199254740992) ~?= Right (-9007199254740992)
    , tryCast @Integer @Double (-9007199254740993) ~?& isLeft
    ]

  -- Word8

  , "Cast Word8 Word16" ~:
    [ cast @Word8 @Word16 0 ~?= 0
    , cast @Word8 @Word16 255 ~?= 255
    ]
  , "Cast Word8 Word32" ~:
    [ cast @Word8 @Word32 0 ~?= 0
    , cast @Word8 @Word32 255 ~?= 255
    ]
  , "Cast Word8 Word64" ~:
    [ cast @Word8 @Word64 0 ~?= 0
    , cast @Word8 @Word64 255 ~?= 255
    ]
  , "Cast Word8 Word" ~:
    [ cast @Word8 @Word 0 ~?= 0
    , cast @Word8 @Word 255 ~?= 255
    ]
  , "Cast Word8 Natural" ~:
    [ cast @Word8 @Natural 0 ~?= 0
    , cast @Word8 @Natural 255 ~?= 255
    ]
  , "TryCast Word8 Int8" ~:
    [ tryCast @Word8 @Int8 0 ~?= Right 0
    , tryCast @Word8 @Int8 127 ~?= Right 127
    , tryCast @Word8 @Int8 128 ~?& isLeft
    ]
  , "Cast Word8 Int16" ~:
    [ cast @Word8 @Int16 0 ~?= 0
    , cast @Word8 @Int16 255 ~?= 255
    ]
  , "Cast Word8 Int32" ~:
    [ cast @Word8 @Int32 0 ~?= 0
    , cast @Word8 @Int32 255 ~?= 255
    ]
  , "Cast Word8 Int64" ~:
    [ cast @Word8 @Int64 0 ~?= 0
    , cast @Word8 @Int64 255 ~?= 255
    ]
  , "Cast Word8 Int" ~:
    [ cast @Word8 @Int 0 ~?= 0
    , cast @Word8 @Int 255 ~?= 255
    ]
  , "Cast Word8 Integer" ~:
    [ cast @Word8 @Integer 0 ~?= 0
    , cast @Word8 @Integer 255 ~?= 255
    ]
  , "Cast Word8 Float" ~:
    [ cast @Word8 @Float 0 ~?= 0
    , cast @Word8 @Float 255 ~?= 255
    ]
  , "Cast Word8 Double" ~:
    [ cast @Word8 @Double 0 ~?= 0
    , cast @Word8 @Double 255 ~?= 255
    ]

  -- Word16

  , "TryCast Word16 Word8" ~:
    [ tryCast @Word16 @Word8 0 ~?= Right 0
    , tryCast @Word16 @Word8 255 ~?= Right 255
    , tryCast @Word16 @Word8 256 ~?& isLeft
    ]
  , "Cast Word16 Word32" ~:
    [ cast @Word16 @Word32 0 ~?= 0
    , cast @Word16 @Word32 65535 ~?= 65535
    ]
  , "Cast Word16 Word64" ~:
    [ cast @Word16 @Word64 0 ~?= 0
    , cast @Word16 @Word64 65535 ~?= 65535
    ]
  , "Cast Word16 Word" ~:
    [ cast @Word16 @Word 0 ~?= 0
    , cast @Word16 @Word 65535 ~?= 65535
    ]
  , "Cast Word16 Natural" ~:
    [ cast @Word16 @Natural 0 ~?= 0
    , cast @Word16 @Natural 65535 ~?= 65535
    ]
  , "TryCast Word16 Int8" ~:
    [ tryCast @Word16 @Int8 0 ~?= Right 0
    , tryCast @Word16 @Int8 127 ~?= Right 127
    , tryCast @Word16 @Int8 128 ~?& isLeft
    ]
  , "TryCast Word16 Int16" ~:
    [ tryCast @Word16 @Int16 0 ~?= Right 0
    , tryCast @Word16 @Int16 32767 ~?= Right 32767
    , tryCast @Word16 @Int16 32768 ~?& isLeft
    ]
  , "Cast Word16 Int32" ~:
    [ cast @Word16 @Int32 0 ~?= 0
    , cast @Word16 @Int32 65535 ~?= 65535
    ]
  , "Cast Word16 Int64" ~:
    [ cast @Word16 @Int64 0 ~?= 0
    , cast @Word16 @Int64 65535 ~?= 65535
    ]
  , "Cast Word16 Int" ~:
    [ cast @Word16 @Int 0 ~?= 0
    , cast @Word16 @Int 65535 ~?= 65535
    ]
  , "Cast Word16 Integer" ~:
    [ cast @Word16 @Integer 0 ~?= 0
    , cast @Word16 @Integer 65535 ~?= 65535
    ]
  , "Cast Word16 Float" ~:
    [ cast @Word16 @Float 0 ~?= 0
    , cast @Word16 @Float 65535 ~?= 65535
    ]
  , "Cast Word16 Double" ~:
    [ cast @Word16 @Double 0 ~?= 0
    , cast @Word16 @Double 65535 ~?= 65535
    ]

  -- Word32

  , "TryCast Word32 Word8" ~:
    [ tryCast @Word32 @Word8 0 ~?= Right 0
    , tryCast @Word32 @Word8 255 ~?= Right 255
    , tryCast @Word32 @Word8 256 ~?& isLeft
    ]
  , "TryCast Word32 Word16" ~:
    [ tryCast @Word32 @Word16 0 ~?= Right 0
    , tryCast @Word32 @Word16 65535 ~?= Right 65535
    , tryCast @Word32 @Word16 65536 ~?& isLeft
    ]
  , "Cast Word32 Word64" ~:
    [ cast @Word32 @Word64 0 ~?= 0
    , cast @Word32 @Word64 4294967295 ~?= 4294967295
    ]
  , "TryCast Word32 Word" ~:
    if toInteger (maxBound :: Word) < 4294967295 then untested else
      [ tryCast @Word32 @Word 0 ~?= Right 0
      , tryCast @Word32 @Word 4294967295 ~?= Right 4294967295
      ]
  , "Cast Word32 Natural" ~:
    [ cast @Word32 @Natural 0 ~?= 0
    , cast @Word32 @Natural 4294967295 ~?= 4294967295
    ]
  , "TryCast Word32 Int8" ~:
    [ tryCast @Word32 @Int8 0 ~?= Right 0
    , tryCast @Word32 @Int8 127 ~?= Right 127
    , tryCast @Word32 @Int8 128 ~?& isLeft
    ]
  , "TryCast Word32 Int16" ~:
    [ tryCast @Word32 @Int16 0 ~?= Right 0
    , tryCast @Word32 @Int16 32767 ~?= Right 32767
    , tryCast @Word32 @Int16 32768 ~?& isLeft
    ]
  , "TryCast Word32 Int32" ~:
    [ tryCast @Word32 @Int32 0 ~?= Right 0
    , tryCast @Word32 @Int32 2147483647 ~?= Right 2147483647
    , tryCast @Word32 @Int32 2147483648 ~?& isLeft
    ]
  , "Cast Word32 Int64" ~:
    [ cast @Word32 @Int64 0 ~?= 0
    , cast @Word32 @Int64 4294967295 ~?= 4294967295
    ]
  , "TryCast Word32 Int" ~:
    if toInteger (maxBound :: Int) < 4294967295 then untested else
      [ tryCast @Word32 @Int 0 ~?= Right 0
      , tryCast @Word32 @Int 4294967295 ~?= Right 4294967295
      ]
  , "Cast Word32 Integer" ~:
    [ cast @Word32 @Integer 0 ~?= 0
    , cast @Word32 @Integer 4294967295 ~?= 4294967295
    ]
  , "TryCast Word32 Float" ~:
    [ tryCast @Word32 @Float 0 ~?= Right 0
    , tryCast @Word32 @Float 16777216 ~?= Right 16777216
    , tryCast @Word32 @Float 16777217 ~?& isLeft
    ]
  , "Cast Word32 Double" ~:
    [ cast @Word32 @Double 0 ~?= 0
    , cast @Word32 @Double 4294967295 ~?= 4294967295
    ]

  -- Word64

  , "TryCast Word64 Word8" ~:
    [ tryCast @Word64 @Word8 0 ~?= Right 0
    , tryCast @Word64 @Word8 255 ~?= Right 255
    , tryCast @Word64 @Word8 256 ~?& isLeft
    ]
  , "TryCast Word64 Word16" ~:
    [ tryCast @Word64 @Word16 0 ~?= Right 0
    , tryCast @Word64 @Word16 65535 ~?= Right 65535
    , tryCast @Word64 @Word16 65536 ~?& isLeft
    ]
  , "TryCast Word64 Word32" ~:
    [ tryCast @Word64 @Word32 0 ~?= Right 0
    , tryCast @Word64 @Word32 4294967295 ~?= Right 4294967295
    , tryCast @Word64 @Word32 4294967296 ~?& isLeft
    ]
  , "TryCast Word64 Word" ~:
    if toInteger (maxBound :: Word) < 18446744073709551615 then untested else
      [ tryCast @Word64 @Word 0 ~?= Right 0
      , tryCast @Word64 @Word 18446744073709551615 ~?= Right 18446744073709551615
      ]
  , "Cast Word64 Natural" ~:
    [ cast @Word64 @Natural 0 ~?= 0
    , cast @Word64 @Natural 18446744073709551615 ~?= 18446744073709551615
    ]
  , "TryCast Word64 Int8" ~:
    [ tryCast @Word64 @Int8 0 ~?= Right 0
    , tryCast @Word64 @Int8 127 ~?= Right 127
    , tryCast @Word64 @Int8 128 ~?& isLeft
    ]
  , "TryCast Word64 Int16" ~:
    [ tryCast @Word64 @Int16 0 ~?= Right 0
    , tryCast @Word64 @Int16 32767 ~?= Right 32767
    , tryCast @Word64 @Int16 32768 ~?& isLeft
    ]
  , "TryCast Word64 Int32" ~:
    [ tryCast @Word64 @Int32 0 ~?= Right 0
    , tryCast @Word64 @Int32 2147483647 ~?= Right 2147483647
    , tryCast @Word64 @Int32 2147483648 ~?& isLeft
    ]
  , "TryCast Word64 Int64" ~:
    [ tryCast @Word64 @Int64 0 ~?= Right 0
    , tryCast @Word64 @Int64 9223372036854775807 ~?= Right 9223372036854775807
    , tryCast @Word64 @Int32 9223372036854775808 ~?& isLeft
    ]
  , "TryCast Word64 Int" ~:
    [ tryCast @Word64 @Int 0 ~?= Right 0
    , let x = maxBound :: Int
      in tryCast @Word64 @Int (fromIntegral x) ~?= Right x
    , let x = fromIntegral (maxBound :: Int) + 1 :: Word64
      in tryCast @Word64 @Int x ~?& isLeft
    ]
  , "Cast Word64 Integer" ~:
    [ cast @Word64 @Integer 0 ~?= 0
    , cast @Word64 @Integer 18446744073709551615 ~?= 18446744073709551615
    ]
  , "TryCast Word64 Float" ~:
    [ tryCast @Word64 @Float 0 ~?= Right 0
    , tryCast @Word64 @Float 16777216 ~?= Right 16777216
    , tryCast @Word64 @Float 16777217 ~?& isLeft
    ]
  , "TryCast Word64 Double" ~:
    [ tryCast @Word64 @Double 0 ~?= Right 0
    , tryCast @Word64 @Double 9007199254740992 ~?= Right 9007199254740992
    , tryCast @Word64 @Double 9007199254740993 ~?& isLeft
    ]

  -- Word

  , "TryCast Word Word8" ~:
    [ tryCast @Word @Word8 0 ~?= Right 0
    , tryCast @Word @Word8 255 ~?= Right 255
    , tryCast @Word @Word8 256 ~?& isLeft
    ]
  , "TryCast Word Word16" ~:
    [ tryCast @Word @Word16 0 ~?= Right 0
    , tryCast @Word @Word16 65535 ~?= Right 65535
    , tryCast @Word @Word16 65536 ~?& isLeft
    ]
  , "TryCast Word Word32" ~:
    if toInteger (maxBound :: Word) < 4294967295 then untested else
      [ tryCast @Word @Word32 0 ~?= Right 0
      , tryCast @Word @Word32 4294967295 ~?= Right 4294967295
      , tryCast @Word @Word32 4294967296 ~?& isLeft
      ]
  , "Cast Word Word64" ~:
    [ cast @Word @Word64 0 ~?= 0
    , cast @Word @Word64 maxBound ~?= fromIntegral (maxBound :: Word)
    ]
  , "Cast Word Natural" ~:
    [ cast @Word @Natural 0 ~?= 0
    , cast @Word @Natural maxBound ~?= fromIntegral (maxBound :: Word)
    ]
  , "TryCast Word Int8" ~:
    [ tryCast @Word @Int8 0 ~?= Right 0
    , tryCast @Word @Int8 127 ~?= Right 127
    , tryCast @Word @Int8 128 ~?& isLeft
    ]
  , "TryCast Word Int16" ~:
    [ tryCast @Word @Int16 0 ~?= Right 0
    , tryCast @Word @Int16 32767 ~?= Right 32767
    , tryCast @Word @Int16 32768 ~?& isLeft
    ]
  , "TryCast Word Int32" ~:
    if toInteger (maxBound :: Word) < 2147483647 then untested else
      [ tryCast @Word @Int32 0 ~?= Right 0
      , tryCast @Word @Int32 2147483647 ~?= Right 2147483647
      , tryCast @Word @Int32 2147483648 ~?& isLeft
      ]
  , "TryCast Word Int64" ~:
    if toInteger (maxBound :: Word) < 9223372036854775807 then untested else
      [ tryCast @Word @Int64 0 ~?= Right 0
      , tryCast @Word @Int64 9223372036854775807 ~?= Right 9223372036854775807
      , tryCast @Word @Int32 9223372036854775808 ~?& isLeft
      ]
  , "TryCast Word Int" ~:
    [ tryCast @Word @Int 0 ~?= Right 0
    , let x = maxBound :: Int
      in tryCast @Word @Int (fromIntegral x) ~?= Right x
    , let x = fromIntegral (maxBound :: Int) + 1 :: Word
      in tryCast @Word @Int x ~?& isLeft
    ]
  , "Cast Word Integer" ~:
    [ cast @Word @Integer 0 ~?= 0
    , cast @Word @Integer maxBound ~?= fromIntegral (maxBound :: Word)
    ]
  , "TryCast Word Float" ~:
    [ tryCast @Word @Float 0 ~?= Right 0
    , tryCast @Word @Float 16777216 ~?= Right 16777216
    , tryCast @Word @Float 16777217 ~?& isLeft
    ]
  , "TryCast Word Double" ~:
    if toInteger (maxBound :: Word) <= 9007199254740992 then untested else
    [ tryCast @Word @Double 0 ~?= Right 0
    , tryCast @Word @Double 9007199254740992 ~?= Right 9007199254740992
    , tryCast @Word @Double 9007199254740993 ~?& isLeft
    ]

  -- Natural

  , "TryCast Natural Word8" ~:
    [ tryCast @Natural @Word8 0 ~?= Right 0
    , tryCast @Natural @Word8 255 ~?= Right 255
    , tryCast @Natural @Word8 256 ~?& isLeft
    ]
  , "TryCast Natural Word16" ~:
    [ tryCast @Natural @Word16 0 ~?= Right 0
    , tryCast @Natural @Word16 65535 ~?= Right 65535
    , tryCast @Natural @Word16 65536 ~?& isLeft
    ]
  , "TryCast Natural Word32" ~:
    [ tryCast @Natural @Word32 0 ~?= Right 0
    , tryCast @Natural @Word32 4294967295 ~?= Right 4294967295
    , tryCast @Natural @Word32 4294967296 ~?& isLeft
    ]
  , "TryCast Natural Word64" ~:
    [ tryCast @Natural @Word64 0 ~?= Right 0
    , tryCast @Natural @Word64 18446744073709551615 ~?= Right 18446744073709551615
    , tryCast @Natural @Word64 18446744073709551616 ~?& isLeft
    ]
  , "TryCast Natural Word" ~:
    [ tryCast @Natural @Word 0 ~?= Right 0
    , let x = maxBound :: Word
      in tryCast @Natural @Word (fromIntegral x) ~?= Right x
    , let x = fromIntegral (maxBound :: Word) + 1 :: Natural
      in tryCast @Natural @Word x ~?& isLeft
    ]
  , "TryCast Natural Int8" ~:
    [ tryCast @Natural @Int8 0 ~?= Right 0
    , tryCast @Natural @Int8 127 ~?= Right 127
    , tryCast @Natural @Int8 128 ~?& isLeft
    ]
  , "TryCast Natural Int16" ~:
    [ tryCast @Natural @Int16 0 ~?= Right 0
    , tryCast @Natural @Int16 32767 ~?= Right 32767
    , tryCast @Natural @Int16 32768 ~?& isLeft
    ]
  , "TryCast Natural Int32" ~:
    [ tryCast @Natural @Int32 0 ~?= Right 0
    , tryCast @Natural @Int32 2147483647 ~?= Right 2147483647
    , tryCast @Natural @Int32 2147483648 ~?& isLeft
    ]
  , "TryCast Natural Int64" ~:
    [ tryCast @Natural @Int64 0 ~?= Right 0
    , tryCast @Natural @Int64 9223372036854775807 ~?= Right 9223372036854775807
    , tryCast @Natural @Int32 9223372036854775808 ~?& isLeft
    ]
  , "TryCast Natural Int" ~:
    [ tryCast @Natural @Int 0 ~?= Right 0
    , let x = maxBound :: Int
      in tryCast @Natural @Int (fromIntegral x) ~?= Right x
    , let x = fromIntegral (maxBound :: Int) + 1 :: Natural
      in tryCast @Natural @Int x ~?& isLeft
    ]
  , "Cast Natural Integer" ~:
    [ cast @Natural @Integer 0 ~?= 0
    , cast @Natural @Integer 9223372036854775808 ~?= 9223372036854775808
    ]
  , "TryCast Natural Float" ~:
    [ tryCast @Natural @Float 0 ~?= Right 0
    , tryCast @Natural @Float 16777216 ~?= Right 16777216
    , tryCast @Natural @Float 16777217 ~?& isLeft
    ]
  , "TryCast Natural Double" ~:
    [ tryCast @Natural @Double 0 ~?= Right 0
    , tryCast @Natural @Double 9007199254740992 ~?= Right 9007199254740992
    , tryCast @Natural @Double 9007199254740993 ~?& isLeft
    ]

  -- Ratio

  , "Cast a (Ratio a)" ~:
    [ cast @Integer @Rational 0 ~?= 0
    , cast @Int @(Ratio Int) 0 ~?= 0
    ]
  , "TryCast (Ratio a) a" ~:
    [ tryCast @Rational @Integer 0 ~?= Right 0
    , tryCast @Rational @Integer 0.5 ~?& isLeft
    , tryCast @(Ratio Int) @Int 0 ~?= Right 0
    , tryCast @(Ratio Int) @Int 0.5 ~?& isLeft
    ]

  -- Fixed

  , "Cast Integer (Fixed a)" ~:
    [ cast @Integer @Uni 1 ~?= 1
    , cast @Integer @Deci 1 ~?= 0.1
    ]
  , "Cast (Fixed a) Integer" ~:
    [ cast @Uni @Integer 1 ~?= 1
    , cast @Deci @Integer 1 ~?= 10
    ]

  -- Complex

  , "Cast a (Complex a)" ~:
    [ cast @Float @(Complex Float) 1 ~?= 1
    , cast @Double @(Complex Double) 1 ~?= 1
    ]
  , "TryCast (Complex a) a" ~:
    [ tryCast @(Complex Float) @Float 1 ~?= Right 1
    , tryCast @(Complex Float) @Float (0 :+ 1) ~?& isLeft
    , tryCast @(Complex Double) @Double 1 ~?= Right 1
    , tryCast @(Complex Double) @Double (0 :+ 1) ~?& isLeft
    ]

  ]

(~?&) :: Show a => a -> (a -> Bool) -> Test
x ~?& f = f x ~? show x
infix 1 ~?&

untested :: [Test]
untested = [False ~? "untested"]

newtype Name
  = Name String
  deriving (Eq, Show)

instance Cast Name String

instance Cast String Name
