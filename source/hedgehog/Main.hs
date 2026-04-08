{-# LANGUAGE DerivingVia #-}

import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Bits as Bits
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
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Ratio as Ratio
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Time as Time
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
    groupDouble,
    groupComplex,
    groupRatio,
    groupFixed,
    groupTagged,
    groupNonEmpty,
    groupSet,
    groupIntSet,
    groupMap,
    groupIntMap,
    groupSeq,
    groupByteString,
    groupLazyByteString,
    groupShortByteString,
    groupText,
    groupLazyText,
    groupString,
    groupMonoid,
    groupSemigroup,
    groupBool,
    groupTime
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

groupComplex :: H.Group
groupComplex = group "Complex" $ do
  property "Integer" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Integer
    let t = Typeable.Proxy :: Typeable.Proxy (Complex.Complex Integer)
    fromTryFrom s t . Gen.integral $ Range.linear (-1000) 1000

groupRatio :: H.Group
groupRatio = group "Ratio" $ do
  property "Integer" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Integer
    let t = Typeable.Proxy :: Typeable.Proxy (Ratio.Ratio Integer)
    fromTryFrom s t . Gen.integral $ Range.linear (-1000) 1000

groupFixed :: H.Group
groupFixed = group "Fixed" $ do
  property "Integer" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Integer
    let t = Typeable.Proxy :: Typeable.Proxy Fixed.Pico
    fromTryFrom s t . Gen.integral $ Range.linear (-1000000000000) 1000000000000

  property "Rational" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Fixed.Pico
    let t = Typeable.Proxy :: Typeable.Proxy Rational
    fromTryFrom s t $ genPico

groupTagged :: H.Group
groupTagged = group "Tagged" $ do
  let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
  let t = Typeable.Proxy :: Typeable.Proxy (Tagged.Tagged () Word.Word8)
  property "a" $ do
    fromFrom s t $ Gen.integral Range.linearBounded

groupNonEmpty :: H.Group
groupNonEmpty = group "NonEmpty" $ do
  property "[Word8]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy (NonEmpty.NonEmpty Word.Word8)
    let t = Typeable.Proxy :: Typeable.Proxy [Word.Word8]
    fromTryFrom s t . Gen.nonEmpty (Range.linear 1 10) $ Gen.integral Range.linearBounded

groupSet :: H.Group
groupSet = group "Set" $ do
  property "[Word8]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy (Set.Set Word.Word8)
    let t = Typeable.Proxy :: Typeable.Proxy [Word.Word8]
    fromFrom s t . Gen.set (Range.linear 0 10) $ Gen.integral Range.linearBounded

groupIntSet :: H.Group
groupIntSet = group "IntSet" $ do
  property "[Int]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy [Int]
    let t = Typeable.Proxy :: Typeable.Proxy IntSet.IntSet
    fromFrom s t . Gen.list (Range.linear 0 10) . Gen.integral $ Range.linear (-100) 100

groupMap :: H.Group
groupMap = group "Map" $ do
  property "[(Word8, Word8)]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy [(Word.Word8, Word.Word8)]
    let t = Typeable.Proxy :: Typeable.Proxy (Map.Map Word.Word8 Word.Word8)
    fromFrom s t . Gen.list (Range.linear 0 10) $ (,) <$> Gen.integral Range.linearBounded <*> Gen.integral Range.linearBounded

groupIntMap :: H.Group
groupIntMap = group "IntMap" $ do
  property "[(Int, Word8)]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy [(Int, Word.Word8)]
    let t = Typeable.Proxy :: Typeable.Proxy (IntMap.IntMap Word.Word8)
    fromFrom s t . Gen.list (Range.linear 0 10) $ (,) <$> Gen.integral (Range.linear (-100) 100) <*> Gen.integral Range.linearBounded

groupSeq :: H.Group
groupSeq = group "Seq" $ do
  property "[Word8]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy [Word.Word8]
    let t = Typeable.Proxy :: Typeable.Proxy (Seq.Seq Word.Word8)
    fromFrom s t . Gen.list (Range.linear 0 10) $ Gen.integral Range.linearBounded

groupByteString :: H.Group
groupByteString = group "ByteString" $ do
  property "[Word8]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy ByteString.ByteString
    let t = Typeable.Proxy :: Typeable.Proxy [Word.Word8]
    fromFrom s t $ genByteString

  property "LazyByteString" $ do
    let s = Typeable.Proxy :: Typeable.Proxy ByteString.ByteString
    let t = Typeable.Proxy :: Typeable.Proxy LazyByteString.ByteString
    fromFrom s t $ genByteString

  property "ShortByteString" $ do
    let s = Typeable.Proxy :: Typeable.Proxy ByteString.ByteString
    let t = Typeable.Proxy :: Typeable.Proxy ShortByteString.ShortByteString
    fromFrom s t $ genByteString

groupLazyByteString :: H.Group
groupLazyByteString = group "LazyByteString" $ do
  property "[Word8]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy LazyByteString.ByteString
    let t = Typeable.Proxy :: Typeable.Proxy [Word.Word8]
    fromFrom s t $ genLazyByteString

groupShortByteString :: H.Group
groupShortByteString = group "ShortByteString" $ do
  property "[Word8]" $ do
    let s = Typeable.Proxy :: Typeable.Proxy ShortByteString.ShortByteString
    let t = Typeable.Proxy :: Typeable.Proxy [Word.Word8]
    fromFrom s t $ genShortByteString

groupText :: H.Group
groupText = group "Text" $ do
  property "LazyText" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Text.Text
    let t = Typeable.Proxy :: Typeable.Proxy LazyText.Text
    fromFrom s t genText

  property "String" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Text.Text
    let t = Typeable.Proxy :: Typeable.Proxy String
    fromFrom s t genText

groupLazyText :: H.Group
groupLazyText = group "LazyText" $ do
  property "String" $ do
    let s = Typeable.Proxy :: Typeable.Proxy LazyText.Text
    let t = Typeable.Proxy :: Typeable.Proxy String
    fromFrom s t genLazyText

groupString :: H.Group
groupString = group "String" $ do
  -- String -> Text -> String and String -> LazyText -> String are already
  -- covered by the Text and LazyText groups. This group is here for
  -- completeness to show that no String-as-source tests are missing.
  pure ()

groupMonoid :: H.Group
groupMonoid = group "Monoid" $ do
  property "Dual" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
    let t = Typeable.Proxy :: Typeable.Proxy (Monoid.Dual Word.Word8)
    fromFrom s t $ Gen.integral Range.linearBounded

  property "Sum" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
    let t = Typeable.Proxy :: Typeable.Proxy (Monoid.Sum Word.Word8)
    fromFrom s t $ Gen.integral Range.linearBounded

  property "Product" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
    let t = Typeable.Proxy :: Typeable.Proxy (Monoid.Product Word.Word8)
    fromFrom s t $ Gen.integral Range.linearBounded

  property "First" $ do
    let s = Typeable.Proxy :: Typeable.Proxy (Maybe Word.Word8)
    let t = Typeable.Proxy :: Typeable.Proxy (Monoid.First Word.Word8)
    fromFrom s t $ Gen.maybe (Gen.integral Range.linearBounded)

  property "Last" $ do
    let s = Typeable.Proxy :: Typeable.Proxy (Maybe Word.Word8)
    let t = Typeable.Proxy :: Typeable.Proxy (Monoid.Last Word.Word8)
    fromFrom s t $ Gen.maybe (Gen.integral Range.linearBounded)

  property "Alt" $ do
    let s = Typeable.Proxy :: Typeable.Proxy (Maybe Word.Word8)
    let t = Typeable.Proxy :: Typeable.Proxy (Monoid.Alt Maybe Word.Word8)
    fromFrom s t $ Gen.maybe (Gen.integral Range.linearBounded)

  property "Ap" $ do
    let s = Typeable.Proxy :: Typeable.Proxy (Maybe Word.Word8)
    let t = Typeable.Proxy :: Typeable.Proxy (Monoid.Ap Maybe Word.Word8)
    fromFrom s t $ Gen.maybe (Gen.integral Range.linearBounded)

  -- Endo cannot be tested because functions are not `Eq` or `Show`.

groupSemigroup :: H.Group
groupSemigroup = group "Semigroup" $ do
  property "Min" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
    let t = Typeable.Proxy :: Typeable.Proxy (Semigroup.Min Word.Word8)
    fromFrom s t $ Gen.integral Range.linearBounded

  property "Max" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
    let t = Typeable.Proxy :: Typeable.Proxy (Semigroup.Max Word.Word8)
    fromFrom s t $ Gen.integral Range.linearBounded

  property "First" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
    let t = Typeable.Proxy :: Typeable.Proxy (Semigroup.First Word.Word8)
    fromFrom s t $ Gen.integral Range.linearBounded

  property "Last" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Word.Word8
    let t = Typeable.Proxy :: Typeable.Proxy (Semigroup.Last Word.Word8)
    fromFrom s t $ Gen.integral Range.linearBounded

groupBool :: H.Group
groupBool = group "Bool" $ do
  property "All" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Bool
    let t = Typeable.Proxy :: Typeable.Proxy Monoid.All
    fromFrom s t Gen.bool

  property "Any" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Bool
    let t = Typeable.Proxy :: Typeable.Proxy Monoid.Any
    fromFrom s t Gen.bool

groupTime :: H.Group
groupTime = group "Time" $ do
  property "Day/Integer" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Integer
    let t = Typeable.Proxy :: Typeable.Proxy Time.Day
    fromFrom s t . Gen.integral $ Range.linear (-100000) 100000

  property "UniversalTime/Rational" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Rational
    let t = Typeable.Proxy :: Typeable.Proxy Time.UniversalTime
    fromFrom s t genRational

  property "DiffTime/Pico" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Fixed.Pico
    let t = Typeable.Proxy :: Typeable.Proxy Time.DiffTime
    fromFrom s t genPico

  property "NominalDiffTime/Pico" $ do
    let s = Typeable.Proxy :: Typeable.Proxy Fixed.Pico
    let t = Typeable.Proxy :: Typeable.Proxy Time.NominalDiffTime
    fromFrom s t genPico

  -- Day -> DayOfWeek cannot be tested because there is no reverse instance.
  -- DayOfWeek is a many-to-one mapping (7 days of the week).

  -- The following time conversions are one-directional and cannot be
  -- roundtrip tested: SystemTime -> UTCTime, SystemTime -> POSIXTime,
  -- SystemTime -> AbsoluteTime, UTCTime -> POSIXTime, POSIXTime -> UTCTime,
  -- UTCTime -> SystemTime, ZonedTime -> UTCTime,
  -- CalendarDiffDays -> CalendarDiffTime, NominalDiffTime -> CalendarDiffTime.

genByteString :: H.Gen ByteString.ByteString
genByteString =
  fmap ByteString.pack . Gen.list (Range.linear 0 20) $ Gen.integral Range.linearBounded

genLazyByteString :: H.Gen LazyByteString.ByteString
genLazyByteString =
  fmap LazyByteString.pack . Gen.list (Range.linear 0 20) $ Gen.integral Range.linearBounded

genShortByteString :: H.Gen ShortByteString.ShortByteString
genShortByteString =
  fmap ShortByteString.pack . Gen.list (Range.linear 0 20) $ Gen.integral Range.linearBounded

genText :: H.Gen Text.Text
genText = fmap Text.pack $ Gen.string (Range.linear 0 20) Gen.unicode

genLazyText :: H.Gen LazyText.Text
genLazyText = fmap LazyText.pack $ Gen.string (Range.linear 0 20) Gen.unicode

genPico :: H.Gen Fixed.Pico
genPico = fmap (fromInteger :: Integer -> Fixed.Pico) . Gen.integral $ Range.linear (-1000000000000) 1000000000000

genRational :: H.Gen Rational
genRational = do
  n <- Gen.integral $ Range.linear (-1000000 :: Integer) 1000000
  d <- Gen.integral $ Range.linear (1 :: Integer) 1000
  pure $ n Ratio.% d

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
