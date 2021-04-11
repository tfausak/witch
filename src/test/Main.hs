{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Data.Int
import Data.List.NonEmpty
import Data.Word
import Numeric.Natural
import Test.HUnit
import Witch

main :: IO ()
main = runTestTTAndExit $ "Witch" ~:
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

  -- []

  , "TryCast [a] (NonEmpty a)" ~:
    [ tryCast @[Int] @(NonEmpty Int) [] ~?= Left (TryCastException [])
    , tryCast @[Int] @(NonEmpty Int) [1] ~?= Right (1 :| [])
    , tryCast @[Int] @(NonEmpty Int) [1, 2] ~?= Right (1 :| [2])
    ]

  -- NonEmpty

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

  -- Int16

  , "TryCast Int16 Int8" ~:
    [ tryCast @Int16 @Int8 0 ~?= Right 0
    , tryCast @Int16 @Int8 127 ~?= Right 127
    , tryCast @Int16 @Int8 128 ~?= Left (TryCastException 128)
    , tryCast @Int16 @Int8 (-128) ~?= Right (-128)
    , tryCast @Int16 @Int8 (-129) ~?= Left (TryCastException $ -129)
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

  -- Int32

  , "TryCast Int32 Int8" ~:
    [ tryCast @Int32 @Int8 0 ~?= Right 0
    , tryCast @Int32 @Int8 127 ~?= Right 127
    , tryCast @Int32 @Int8 128 ~?= Left (TryCastException 128)
    , tryCast @Int32 @Int8 (-128) ~?= Right (-128)
    , tryCast @Int32 @Int8 (-129) ~?= Left (TryCastException $ -129)
    ]
  , "TryCast Int32 Int16" ~:
    [ tryCast @Int32 @Int16 0 ~?= Right 0
    , tryCast @Int32 @Int16 32767 ~?= Right 32767
    , tryCast @Int32 @Int16 32768 ~?= Left (TryCastException 32768)
    , tryCast @Int32 @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Int32 @Int16 (-32769) ~?= Left (TryCastException $ -32769)
    ]
  , "Cast Int32 Int64" ~:
    [ cast @Int32 @Int64 0 ~?= 0
    , cast @Int32 @Int64 2147483647 ~?= 2147483647
    , cast @Int32 @Int64 (-2147483648) ~?= -2147483648
    ]
  , "TryCast Int32 Int" ~:
    if toInteger (maxBound :: Int) >= 2147483647 then
      [ tryCast @Int32 @Int 0 ~?= Right 0
      , tryCast @Int32 @Int 2147483647 ~?= Right 2147483647
      , tryCast @Int32 @Int (-2147483648) ~?= Right (-2147483648)
      ]
    else [False ~? "untested"]
  , "Cast Int32 Integer" ~:
    [ cast @Int32 @Integer 0 ~?= 0
    , cast @Int32 @Integer 2147483647 ~?= 2147483647
    , cast @Int32 @Integer (-2147483648) ~?= -2147483648
    ]

  -- Int64

  , "TryCast Int64 Int8" ~:
    [ tryCast @Int64 @Int8 0 ~?= Right 0
    , tryCast @Int64 @Int8 127 ~?= Right 127
    , tryCast @Int64 @Int8 128 ~?= Left (TryCastException 128)
    , tryCast @Int64 @Int8 (-128) ~?= Right (-128)
    , tryCast @Int64 @Int8 (-129) ~?= Left (TryCastException $ -129)
    ]
  , "TryCast Int64 Int16" ~:
    [ tryCast @Int64 @Int16 0 ~?= Right 0
    , tryCast @Int64 @Int16 32767 ~?= Right 32767
    , tryCast @Int64 @Int16 32768 ~?= Left (TryCastException 32768)
    , tryCast @Int64 @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Int64 @Int16 (-32769) ~?= Left (TryCastException $ -32769)
    ]
  , "TryCast Int64 Int32" ~:
    [ tryCast @Int64 @Int32 0 ~?= Right 0
    , tryCast @Int64 @Int32 2147483647 ~?= Right 2147483647
    , tryCast @Int64 @Int32 2147483648 ~?= Left (TryCastException 2147483648)
    , tryCast @Int64 @Int32 (-2147483648) ~?= Right (-2147483648)
    , tryCast @Int64 @Int32 (-2147483649) ~?= Left (TryCastException $ -2147483649)
    ]
  , "TryCast Int64 Int" ~:
    if toInteger (maxBound :: Int) >= 9223372036854775807 then
      [ tryCast @Int64 @Int 0 ~?= Right 0
      , tryCast @Int64 @Int 9223372036854775807 ~?= Right 9223372036854775807
      , tryCast @Int64 @Int (-9223372036854775808) ~?= Right (-9223372036854775808)
      ]
    else [False ~? "untested"]
  , "Cast Int64 Integer" ~:
    [ cast @Int64 @Integer 0 ~?= 0
    , cast @Int64 @Integer 9223372036854775807 ~?= 9223372036854775807
    , cast @Int64 @Integer (-9223372036854775808) ~?= -9223372036854775808
    ]

  -- Int

  , "TryCast Int Int8" ~:
    [ tryCast @Int @Int8 0 ~?= Right 0
    , tryCast @Int @Int8 127 ~?= Right 127
    , tryCast @Int @Int8 128 ~?= Left (TryCastException 128)
    , tryCast @Int @Int8 (-128) ~?= Right (-128)
    , tryCast @Int @Int8 (-129) ~?= Left (TryCastException $ -129)
    ]
  , "TryCast Int Int16" ~:
    [ tryCast @Int @Int16 0 ~?= Right 0
    , tryCast @Int @Int16 32767 ~?= Right 32767
    , tryCast @Int @Int16 32768 ~?= Left (TryCastException 32768)
    , tryCast @Int @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Int @Int16 (-32769) ~?= Left (TryCastException $ -32769)
    ]
  , "TryCast Int Int32" ~:
    if toInteger (maxBound :: Int) >= 2147483647 then
      [ tryCast @Int @Int32 0 ~?= Right 0
      , tryCast @Int @Int32 2147483647 ~?= Right 2147483647
      , tryCast @Int @Int32 2147483648 ~?= Left (TryCastException 2147483648)
      , tryCast @Int @Int32 (-2147483648) ~?= Right (-2147483648)
      , tryCast @Int @Int32 (-2147483649) ~?= Left (TryCastException $ -2147483649)
      ]
    else [False ~? "untested"]
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

  -- Integer

  , "TryCast Integer Int8" ~:
    [ tryCast @Integer @Int8 0 ~?= Right 0
    , tryCast @Integer @Int8 127 ~?= Right 127
    , tryCast @Integer @Int8 128 ~?= Left (TryCastException 128)
    , tryCast @Integer @Int8 (-128) ~?= Right (-128)
    , tryCast @Integer @Int8 (-129) ~?= Left (TryCastException $ -129)
    ]
  , "TryCast Integer Int16" ~:
    [ tryCast @Integer @Int16 0 ~?= Right 0
    , tryCast @Integer @Int16 32767 ~?= Right 32767
    , tryCast @Integer @Int16 32768 ~?= Left (TryCastException 32768)
    , tryCast @Integer @Int16 (-32768) ~?= Right (-32768)
    , tryCast @Integer @Int16 (-32769) ~?= Left (TryCastException $ -32769)
    ]
  , "TryCast Integer Int32" ~:
    [ tryCast @Integer @Int32 0 ~?= Right 0
    , tryCast @Integer @Int32 2147483647 ~?= Right 2147483647
    , tryCast @Integer @Int32 2147483648 ~?= Left (TryCastException 2147483648)
    , tryCast @Integer @Int32 (-2147483648) ~?= Right (-2147483648)
    , tryCast @Integer @Int32 (-2147483649) ~?= Left (TryCastException $ -2147483649)
    ]
  , "TryCast Integer Int64" ~:
    [ tryCast @Integer @Int64 0 ~?= Right 0
    , tryCast @Integer @Int64 9223372036854775807 ~?= Right 9223372036854775807
    , tryCast @Integer @Int64 9223372036854775808 ~?= Left (TryCastException 9223372036854775808)
    , tryCast @Integer @Int64 (-9223372036854775808) ~?= Right (-9223372036854775808)
    , tryCast @Integer @Int64 (-9223372036854775809) ~?= Left (TryCastException $ -9223372036854775809)
    ]
  , "TryCast Integer Int" ~:
    [ tryCast @Integer @Int 0 ~?= Right 0
    , let x = maxBound :: Int
      in tryCast @Integer @Int (fromIntegral x) ~?= Right x
    , let x = toInteger (maxBound :: Int) + 1
      in tryCast @Integer @Int x ~?= Left (TryCastException x)
    , let x = minBound :: Int
      in tryCast @Integer @Int (fromIntegral x) ~?= Right x
    , let x = toInteger (minBound :: Int) - 1
      in tryCast @Integer @Int x ~?= Left (TryCastException x)
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

  -- Word16

  , "TryCast Word16 Word8" ~:
    [ tryCast @Word16 @Word8 0 ~?= Right 0
    , tryCast @Word16 @Word8 255 ~?= Right 255
    , tryCast @Word16 @Word8 256 ~?= Left (TryCastException 256)
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

  -- Word32

  , "TryCast Word32 Word8" ~:
    [ tryCast @Word32 @Word8 0 ~?= Right 0
    , tryCast @Word32 @Word8 255 ~?= Right 255
    , tryCast @Word32 @Word8 256 ~?= Left (TryCastException 256)
    ]
  , "TryCast Word32 Word16" ~:
    [ tryCast @Word32 @Word16 0 ~?= Right 0
    , tryCast @Word32 @Word16 65535 ~?= Right 65535
    , tryCast @Word32 @Word16 65536 ~?= Left (TryCastException 65536)
    ]
  , "Cast Word32 Word64" ~:
    [ cast @Word32 @Word64 0 ~?= 0
    , cast @Word32 @Word64 4294967295 ~?= 4294967295
    ]
  , "TryCast Word32 Word" ~:
    if toInteger (maxBound :: Word) >= 4294967295 then
      [ tryCast @Word32 @Word 0 ~?= Right 0
      , tryCast @Word32 @Word 4294967295 ~?= Right 4294967295
      ]
    else [False ~? "untested"]
  , "Cast Word32 Natural" ~:
    [ cast @Word32 @Natural 0 ~?= 0
    , cast @Word32 @Natural 4294967295 ~?= 4294967295
    ]

  -- Word64

  , "TryCast Word64 Word8" ~:
    [ tryCast @Word64 @Word8 0 ~?= Right 0
    , tryCast @Word64 @Word8 255 ~?= Right 255
    , tryCast @Word64 @Word8 256 ~?= Left (TryCastException 256)
    ]
  , "TryCast Word64 Word16" ~:
    [ tryCast @Word64 @Word16 0 ~?= Right 0
    , tryCast @Word64 @Word16 65535 ~?= Right 65535
    , tryCast @Word64 @Word16 65536 ~?= Left (TryCastException 65536)
    ]
  , "TryCast Word64 Word32" ~:
    [ tryCast @Word64 @Word32 0 ~?= Right 0
    , tryCast @Word64 @Word32 4294967295 ~?= Right 4294967295
    , tryCast @Word64 @Word32 4294967296 ~?= Left (TryCastException 4294967296)
    ]
  , "TryCast Word64 Word" ~:
    if toInteger (maxBound :: Word) >= 18446744073709551615 then
      [ tryCast @Word64 @Word 0 ~?= Right 0
      , tryCast @Word64 @Word 18446744073709551615 ~?= Right 18446744073709551615
      ]
    else [False ~? "untested"]
  , "Cast Word64 Natural" ~:
    [ cast @Word64 @Natural 0 ~?= 0
    , cast @Word64 @Natural 18446744073709551615 ~?= 18446744073709551615
    ]

  -- Word

  , "TryCast Word Word8" ~:
    [ tryCast @Word @Word8 0 ~?= Right 0
    , tryCast @Word @Word8 255 ~?= Right 255
    , tryCast @Word @Word8 256 ~?= Left (TryCastException 256)
    ]
  , "TryCast Word Word16" ~:
    [ tryCast @Word @Word16 0 ~?= Right 0
    , tryCast @Word @Word16 65535 ~?= Right 65535
    , tryCast @Word @Word16 65536 ~?= Left (TryCastException 65536)
    ]
  , "TryCast Word Word32" ~:
    if toInteger (maxBound :: Word) >= 4294967295 then
      [ tryCast @Word @Word32 0 ~?= Right 0
      , tryCast @Word @Word32 4294967295 ~?= Right 4294967295
      , tryCast @Word @Word32 4294967296 ~?= Left (TryCastException 4294967296)
      ]
    else [False ~? "untested"]
  , "Cast Word Word64" ~:
    [ cast @Word @Word64 0 ~?= 0
    , cast @Word @Word64 maxBound ~?= fromIntegral (maxBound :: Word)
    ]
  , "Cast Word Natural" ~:
    [ cast @Word @Natural 0 ~?= 0
    , cast @Word @Natural maxBound ~?= fromIntegral (maxBound :: Word)
    ]

  -- Natural

  , "TryCast Natural Word8" ~:
    [ tryCast @Natural @Word8 0 ~?= Right 0
    , tryCast @Natural @Word8 255 ~?= Right 255
    , tryCast @Natural @Word8 256 ~?= Left (TryCastException 256)
    ]
  , "TryCast Natural Word16" ~:
    [ tryCast @Natural @Word16 0 ~?= Right 0
    , tryCast @Natural @Word16 65535 ~?= Right 65535
    , tryCast @Natural @Word16 65536 ~?= Left (TryCastException 65536)
    ]
  , "TryCast Natural Word32" ~:
    [ tryCast @Natural @Word32 0 ~?= Right 0
    , tryCast @Natural @Word32 4294967295 ~?= Right 4294967295
    , tryCast @Natural @Word32 4294967296 ~?= Left (TryCastException 4294967296)
    ]
  , "TryCast Natural Word64" ~:
    [ tryCast @Natural @Word64 0 ~?= Right 0
    , tryCast @Natural @Word64 18446744073709551615 ~?= Right 18446744073709551615
    , tryCast @Natural @Word64 18446744073709551616 ~?= Left (TryCastException 18446744073709551616)
    ]
  , "TryCast Natural Word" ~:
    [ tryCast @Natural @Word 0 ~?= Right 0
    , let x = maxBound :: Word
      in tryCast @Natural @Word (fromIntegral x) ~?= Right x
    , let x = fromIntegral (maxBound :: Word) + 1 :: Natural
      in tryCast @Natural @Word x ~?= Left (TryCastException x)
    ]

  ]

newtype Name
  = Name String
  deriving (Eq, Show)

instance Cast Name String

instance Cast String Name
