{-# LANGUAGE DerivingVia #-}

import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Typeable as Typeable
import qualified Data.Void as Void
import qualified Data.Word as Word
import qualified GHC.Stack as Stack
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Main
import qualified Hedgehog.Range as Range
import qualified Numeric.Natural as Natural
import qualified Witch

main :: IO ()
main = Main.defaultMain $ fmap H.checkParallel groups

groups :: [H.Group]
groups =
  [ groupA,
    groupList,
    groupInt8,
    groupInt16,
    groupInt32,
    groupInt64,
    groupInt,
    groupInteger,
    groupWord8,
    groupWord16,
    groupWord32,
    groupWord64,
    groupWord,
    groupNatural,
    groupFloat,
    groupDouble
  ]

groupA :: H.Group
groupA = group "a" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Word.Word8

  property "a" $ do
    let t = s
    fromFrom s t $ Gen.integral Range.linearBounded

groupList :: H.Group
groupList = group "List" $ do
  let s = Typeable.Proxy :: Typeable.Proxy [Word.Word8]

  property "Set" $ do
    let t = Typeable.Proxy :: Typeable.Proxy (Set.Set Word.Word8)
    fromFrom s t . Gen.list (Range.linear 0 10) $ Gen.integral Range.linearBounded

groupInt8 :: H.Group
groupInt8 = group "Int8" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Int.Int8

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t . Gen.integral $ Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 maxBound

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupInt16 :: H.Group
groupInt16 = group "Int16" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Int.Int16

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 maxBound

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupInt32 :: H.Group
groupInt32 = group "Int32" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Int.Int32

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 maxBound

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupInt64 :: H.Group
groupInt64 = group "Int64" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Int.Int64

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 maxBound

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupInt :: H.Group
groupInt = group "Int" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Int

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 maxBound

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupInteger :: H.Group
groupInteger = group "Integer" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Integer

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromFrom s t . Gen.integral $ fmap (toInteger . asProxy t) Range.linearBounded

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromFrom s t . Gen.integral $ Range.linear 0 9999999999999999999

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ fmap (toInteger . asProxy f32) Range.linearBounded

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    tryFromTryFrom s t . Gen.integral $ fmap (toInteger . asProxy f64) Range.linearBounded

groupWord8 :: H.Group
groupWord8 = group "Word8" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Word.Word8

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupWord16 :: H.Group
groupWord16 = group "Word16" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Word.Word16

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupWord32 :: H.Group
groupWord32 = group "Word32" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Word.Word32

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupWord64 :: H.Group
groupWord64 = group "Word64" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Word.Word64

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupWord :: H.Group
groupWord = group "Word" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Word

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    fromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    fromTryFrom s t $ Gen.integral Range.linearBounded

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f32) (mkUpperBound s f32)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    tryFromTryFrom s t . Gen.integral $ Range.linear (mkLowerBound s f64) (mkUpperBound s f64)

groupNatural :: H.Group
groupNatural = group "Natural" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Natural.Natural

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    fromTryFrom s t . Gen.integral $ Range.linear 0 9999999999999999999

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy t maxBound)

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy f32 maxBound)

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    tryFromTryFrom s t . Gen.integral $ Range.linear 0 (unsafeFromIntegral $ asProxy f64 maxBound)

groupFloat :: H.Group
groupFloat = group "Float" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Float

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ fmap unF32 Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f32 t) (mkUpperBound f32 t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear 0 (unF32 maxBound)

  property "Rational" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Rational
    tryFromFrom s t . Gen.realFloat $ fmap (realToFrac . unF32) Range.linearBounded

  property "Double" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Double
    fromFrom s t . Gen.realFloat $ fmap (realToFrac . unF32) Range.linearBounded

groupDouble :: H.Group
groupDouble = group "Double" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Double

  property "Int8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int8
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Int16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int16
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Int32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int32
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Int64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int.Int64
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Int" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Int
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Integer" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Integer
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ fmap unF64 Range.linearBounded

  property "Word8" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word8
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Word16" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word16
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Word32" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word32
    tryFromFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Word64" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word.Word64
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Word" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Word
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear (mkLowerBound f64 t) (mkUpperBound f64 t)

  property "Natural" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Natural.Natural
    tryFromTryFrom s t . fmap realToFrac . Gen.integral $ Range.linear 0 (unF64 maxBound)

  property "Rational" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Rational
    tryFromFrom s t . Gen.realFloat $ fmap (realToFrac . unF64) Range.linearBounded

  property "Float" $ do
    let t = Typeable.Proxy :: Typeable.Proxy Float
    fromFrom s t . Gen.realFloat $ fmap (realToFrac . unF32) Range.linearBounded

newtype F32 = MkF32
  { unF32 :: Int.Int32
  }
  deriving (Eq, Ord, Show)
  deriving (Bits.Bits, Enum, Integral, Num, Real) via Int.Int32

instance Bounded F32 where
  minBound = MkF32 (-16777215)
  maxBound = MkF32 16777215

f32 :: Typeable.Proxy F32
f32 = Typeable.Proxy

newtype F64 = MkF64
  { unF64 :: Int.Int64
  }
  deriving (Eq, Ord, Show)
  deriving (Bits.Bits, Enum, Integral, Num, Real) via Int.Int64

instance Bounded F64 where
  minBound = MkF64 (-9007199254740991)
  maxBound = MkF64 9007199254740991

f64 :: Typeable.Proxy F64
f64 = Typeable.Proxy

mkLowerBound ::
  ( Stack.HasCallStack,
    Bits.Bits a,
    Bounded a,
    Bounded b,
    Integral a,
    Integral b
  ) =>
  proxy a ->
  proxy b ->
  a
mkLowerBound a b =
  unsafeFromIntegral $
    max
      (toInteger $ asProxy a minBound)
      (toInteger $ asProxy b minBound)

mkUpperBound ::
  ( Stack.HasCallStack,
    Bits.Bits a,
    Bounded a,
    Bounded b,
    Integral a,
    Integral b
  ) =>
  proxy a ->
  proxy b ->
  a
mkUpperBound a b =
  unsafeFromIntegral $
    min
      (toInteger $ asProxy a maxBound)
      (toInteger $ asProxy b maxBound)

asProxy :: proxy a -> a -> a
asProxy = const id

unsafeFromIntegral ::
  (Stack.HasCallStack, Bits.Bits a, Bits.Bits b, Integral a, Integral b) =>
  a ->
  b
unsafeFromIntegral = Maybe.fromJust . Bits.toIntegralSized

group ::
  String ->
  Writer.Writer [(H.PropertyName, H.Property)] () ->
  H.Group
group n = H.Group (String.fromString n) . Writer.execWriter

property ::
  (Monad m) =>
  String ->
  H.PropertyT IO () ->
  Writer.WriterT [(H.PropertyName, H.Property)] m ()
property n = Writer.tell . pure . (,) (String.fromString n) . H.property

fromFrom ::
  ( Eq source,
    Eq target,
    Witch.From source target,
    Witch.From target source,
    Monad m,
    Show source,
    Show target
  ) =>
  proxy source ->
  proxy target ->
  H.Gen source ->
  H.PropertyT m ()
fromFrom s t =
  tripping
    (right . Witch.from . asProxy s)
    (right . Witch.from . asProxy t)

fromTryFrom ::
  ( Eq source,
    Eq target,
    Witch.From source target,
    Monad m,
    Show source,
    Show target,
    Witch.TryFrom target source,
    Typeable.Typeable source,
    Typeable.Typeable target
  ) =>
  proxy source ->
  proxy target ->
  H.Gen source ->
  H.PropertyT m ()
fromTryFrom s t =
  tripping
    (right . Witch.from . asProxy s)
    (Witch.tryFrom . asProxy t)

tryFromFrom ::
  ( Eq source,
    Eq target,
    Witch.TryFrom source target,
    Witch.From target source,
    Monad m,
    Show source,
    Show target,
    Typeable.Typeable source,
    Typeable.Typeable target
  ) =>
  proxy source ->
  proxy target ->
  H.Gen source ->
  H.PropertyT m ()
tryFromFrom s t =
  tripping
    (Witch.tryFrom . asProxy s)
    (right . Witch.from . asProxy t)

tryFromTryFrom ::
  ( Eq source,
    Eq target,
    Witch.TryFrom source target,
    Witch.TryFrom target source,
    Monad m,
    Show source,
    Show target,
    Typeable.Typeable source,
    Typeable.Typeable target
  ) =>
  proxy source ->
  proxy target ->
  H.Gen source ->
  H.PropertyT m ()
tryFromTryFrom s t =
  tripping
    (Witch.tryFrom . asProxy s)
    (Witch.tryFrom . asProxy t)

tripping ::
  (Eq source, Eq target, Monad m, Show source, Show target, Show e1, Show e2) =>
  (source -> Either e1 target) ->
  (target -> Either e2 source) ->
  H.Gen source ->
  H.PropertyT m ()
tripping into from gen = do
  s1 <- H.forAll gen
  case into s1 of
    Left _ ->
      -- If we can't convert the source into the target, then there's nothing
      -- to test. But we don't want to fail the property with 'H.evalEither'.
      H.discard
    Right t1 -> do
      s2 <- H.evalEither $ from t1
      -- Note that @s2@ is not equal to @s1@ in general because the target type
      -- may lose information. For example if the source is a list and the
      -- target is a set, then @s2@ will be sorted and have no duplicates.

      t2 <- H.evalEither $ into s2
      -- The target should be the same after round-tripping through the source.
      t2 H.=== t1

      s3 <- H.evalEither $ from t2
      -- After the source has been through the target once, round-tripping
      -- through the target /again/ shouldn't change it.
      s3 H.=== s2

right :: a -> Either Void.Void a
right = Right
