{-# LANGUAGE TypeApplications #-}

import Data.Int
import Data.List.NonEmpty
import Test.HUnit
import Witch

main :: IO ()
main = runTestTTAndExit $ "Witch" ~:
  [ "Cast (NonEmpty a) [a]" ~:
    [ cast ('a' :| []) ~?= "a"
    , cast ('a' :| "b") ~?= "ab"
    ]
  , "TryCast [a] (NonEmpty a)" ~:
    [ tryInto @(NonEmpty Char) "" ~?= Left (TryCastException "")
    , tryCast "a" ~?= Right ('a' :| [])
    , tryCast "ab" ~?= Right ('a' :| "b")
    ]
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
  ]
