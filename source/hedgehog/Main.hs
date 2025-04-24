{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
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
  [ groupWitch,
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

groupWitch :: H.Group
groupWitch =
  H.Group
    "witch"
    [ (,) "tripping a a"
        . H.property
        . fromFrom @Word.Word8
        $ Gen.word8 Range.exponentialBounded,
      (,) "tripping (List a) (Set a)"
        . H.property
        . fromFrom @(Set.Set Word.Word8)
        . Gen.list (Range.linear 0 10)
        $ Gen.word8 Range.exponentialBounded
    ]

groupInt8 :: H.Group
groupInt8 = group "Int8" $ do
  let s = p @Int.Int8

  property "Int16" $ do
    let t = p @Int.Int16
    fromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = p @Int.Int32
    fromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = p @Int.Int64
    fromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = p @Int
    fromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = p @Integer
    fromTryFromP s t . Gen.integral $ Range.linearBounded

  property "Word8" $ do
    let t = p @Word.Word8
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = p @Word.Word16
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = p @Word.Word32
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = p @Word.Word64
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = p @Word
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = p @Natural.Natural
    tryFromTryFromP s t . Gen.integral $ Range.linear 0 maxBound

  property "Float" $ do
    let t = p @Float
    fromTryFromP s t $ Gen.integral Range.linearBounded

  property "Double" $ do
    let t = p @Double
    fromTryFromP s t $ Gen.integral Range.linearBounded

groupInt16 :: H.Group
groupInt16 = group "Int16" $ do
  let s = p @Int.Int16

  property "Int8" $ do
    let t = p @Int.Int8
    tryFromFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int32" $ do
    let t = p @Int.Int32
    fromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int64" $ do
    let t = p @Int.Int64
    fromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Int" $ do
    let t = p @Int
    fromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Integer" $ do
    let t = p @Integer
    fromTryFromP s t $ Gen.integral Range.linearBounded

  property "Word8" $ do
    let t = p @Word.Word8
    tryFromFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word16" $ do
    let t = p @Word.Word16
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word32" $ do
    let t = p @Word.Word32
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word64" $ do
    let t = p @Word.Word64
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Word" $ do
    let t = p @Word
    tryFromTryFromP s t . Gen.integral $ Range.linear (mkLowerBound s t) (mkUpperBound s t)

  property "Natural" $ do
    let t = p @Natural.Natural
    tryFromTryFromP s t . Gen.integral $ Range.linear 0 maxBound

  property "Float" $ do
    let t = p @Float
    fromTryFromP s t $ Gen.integral Range.linearBounded

  property "Double" $ do
    let t = p @Double
    fromTryFromP s t $ Gen.integral Range.linearBounded

groupInt32 :: H.Group
groupInt32 = group "Int32" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Int.Int32
  property "Int8" . tryFromFrom @Int.Int8 $ gen -128 127
  property "Int16" . tryFromFrom @Int.Int16 $ gen -32768 32767
  property "Int64" . fromTryFrom @Int.Int64 $ gen -2147483648 2147483647
  property "Int" . tryFromTryFrom @Int $ gen -2147483648 2147483647
  property "Integer" . fromTryFrom @Integer $ gen -2147483648 2147483647
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word32" . tryFromTryFrom @Word.Word32 $ gen 0 2147483647
  property "Word64" . tryFromTryFrom @Word.Word64 $ gen 0 2147483647
  property "Word" . tryFromTryFrom @Word $ gen 0 2147483647
  property "Natural" . tryFromTryFrom @Natural.Natural $ gen 0 2147483647
  property "Float" . tryFromTryFrom @Float $ gen -16777215 16777215
  property "Double" . fromTryFrom @Double $ gen -2147483648 2147483647

groupInt64 :: H.Group
groupInt64 = group "Int64" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Int.Int64
  property "Int8" . tryFromFrom @Int.Int8 $ gen -128 127
  property "Int16" . tryFromFrom @Int.Int16 $ gen -32768 32767
  property "Int32" . tryFromFrom @Int.Int32 $ gen -2147483648 2147483647
  property "Int" . tryFromFrom @Int $ gen -9223372036854775808 9223372036854775807
  property "Integer" . fromTryFrom @Integer $ gen -9223372036854775808 9223372036854775807
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word32" . tryFromFrom @Word.Word32 $ gen 0 4294967295
  property "Word64" . tryFromTryFrom @Word.Word64 $ gen 0 9223372036854775807
  property "Word" . tryFromTryFrom @Word $ gen 0 9223372036854775807
  property "Natural" . tryFromTryFrom @Natural.Natural $ gen 0 9223372036854775807
  property "Float" . tryFromTryFrom @Float $ gen -16777215 16777215
  property "Double" . tryFromTryFrom @Double $ gen -9007199254740991 9007199254740991

groupInt :: H.Group
groupInt = group "Int" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Int
  property "Int8" . tryFromFrom @Int.Int8 $ gen -128 127
  property "Int16" . tryFromFrom @Int.Int16 $ gen -32768 32767
  property "Int32" . tryFromTryFrom @Int.Int32 $ gen -2147483648 2147483647
  property "Int64" . fromTryFrom @Int.Int64 $ gen -9223372036854775808 9223372036854775807
  property "Integer" . fromTryFrom @Integer $ gen -9223372036854775808 9223372036854775807
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word32" . tryFromTryFrom @Word.Word32 $ gen 0 4294967295
  property "Word64" . tryFromTryFrom @Word.Word64 $ gen 0 9223372036854775807
  property "Word" . tryFromTryFrom @Word $ gen 0 9223372036854775807
  property "Natural" . tryFromTryFrom @Natural.Natural $ gen 0 9223372036854775807
  property "Float" . tryFromTryFrom @Float $ gen -16777215 16777215
  property "Double" . tryFromTryFrom @Double $ gen -9007199254740991 9007199254740991

groupInteger :: H.Group
groupInteger = group "Integer" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Integer
  property "Int8" . tryFromFrom @Int.Int8 $ gen -128 127
  property "Int16" . tryFromFrom @Int.Int16 $ gen -32768 32767
  property "Int32" . tryFromFrom @Int.Int32 $ gen -2147483648 2147483647
  property "Int64" . tryFromFrom @Int.Int64 $ gen -9223372036854775808 9223372036854775807
  property "Int" . tryFromFrom @Int $ gen -9223372036854775808 9223372036854775807
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word32" . tryFromFrom @Word.Word32 $ gen 0 4294967295
  property "Word64" . tryFromFrom @Word.Word64 $ gen 0 18446744073709551615
  property "Word" . tryFromFrom @Word $ gen 0 18446744073709551615
  property "Natural" . tryFromFrom @Natural.Natural $ gen 0 99999999999999999999
  property "Float" . tryFromTryFrom @Float $ gen -16777215 16777215
  property "Double" . tryFromTryFrom @Double $ gen -9007199254740991 9007199254740991

groupWord8 :: H.Group
groupWord8 = group "Word8" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Word.Word8
  property "Int8" . tryFromTryFrom @Int.Int8 $ gen 0 127
  property "Int16" . fromTryFrom @Int.Int16 $ gen 0 255
  property "Int32" . fromTryFrom @Int.Int32 $ gen 0 255
  property "Int64" . fromTryFrom @Int.Int64 $ gen 0 255
  property "Int" . fromTryFrom @Int $ gen 0 255
  property "Integer" . fromTryFrom @Integer $ gen 0 255
  property "Word16" . fromTryFrom @Word.Word16 $ gen 0 255
  property "Word32" . fromTryFrom @Word.Word32 $ gen 0 255
  property "Word64" . fromTryFrom @Word.Word64 $ gen 0 255
  property "Word" . fromTryFrom @Word $ gen 0 255
  property "Natural" . fromTryFrom @Natural.Natural $ gen 0 255
  property "Float" . fromTryFrom @Float $ gen 0 255
  property "Double" . fromTryFrom @Double $ gen 0 255

groupWord16 :: H.Group
groupWord16 = group "Word16" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Word.Word16
  property "Int8" . tryFromTryFrom @Int.Int8 $ gen 0 127
  property "Int16" . tryFromTryFrom @Int.Int16 $ gen 0 32767
  property "Int32" . fromTryFrom @Int.Int32 $ gen 0 65535
  property "Int64" . fromTryFrom @Int.Int64 $ gen 0 65535
  property "Int" . fromTryFrom @Int $ gen 0 65535
  property "Integer" . fromTryFrom @Integer $ gen 0 65535
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word32" . fromTryFrom @Word.Word32 $ gen 0 65535
  property "Word64" . fromTryFrom @Word.Word64 $ gen 0 65535
  property "Word" . fromTryFrom @Word $ gen 0 65535
  property "Natural" . fromTryFrom @Natural.Natural $ gen 0 65535
  property "Float" . fromTryFrom @Float $ gen 0 65535
  property "Double" . fromTryFrom @Double $ gen 0 65535

groupWord32 :: H.Group
groupWord32 = group "Word32" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Word.Word32
  property "Int8" . tryFromTryFrom @Int.Int8 $ gen 0 127
  property "Int16" . tryFromTryFrom @Int.Int16 $ gen 0 32767
  property "Int32" . tryFromTryFrom @Int.Int32 $ gen 0 2147483647
  property "Int64" . fromTryFrom @Int.Int64 $ gen 0 4294967295
  property "Int" . tryFromTryFrom @Int $ gen 0 4294967295
  property "Integer" . fromTryFrom @Integer $ gen 0 4294967295
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word64" . fromTryFrom @Word.Word64 $ gen 0 4294967295
  property "Word" . tryFromTryFrom @Word $ gen 0 4294967295
  property "Natural" . fromTryFrom @Natural.Natural $ gen 0 4294967295
  property "Float" . tryFromTryFrom @Float $ gen 0 16777215
  property "Double" . fromTryFrom @Double $ gen 0 4294967295

groupWord64 :: H.Group
groupWord64 = group "Word64" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Word.Word64
  property "Int8" . tryFromTryFrom @Int.Int8 $ gen 0 127
  property "Int16" . tryFromTryFrom @Int.Int16 $ gen 0 32767
  property "Int32" . tryFromTryFrom @Int.Int32 $ gen 0 2147483647
  property "Int64" . tryFromTryFrom @Int.Int64 $ gen 0 9223372036854775807
  property "Int" . tryFromTryFrom @Int $ gen 0 9223372036854775807
  property "Integer" . fromTryFrom @Integer $ gen 0 18446744073709551615
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word32" . tryFromFrom @Word.Word32 $ gen 0 4294967295
  property "Word" . tryFromFrom @Word $ gen 0 18446744073709551615
  property "Natural" . fromTryFrom @Natural.Natural $ gen 0 18446744073709551615
  property "Float" . tryFromTryFrom @Float $ gen 0 16777215
  property "Double" . tryFromTryFrom @Double $ gen 0 9007199254740991

groupWord :: H.Group
groupWord = group "Word" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Word
  property "Int8" . tryFromTryFrom @Int.Int8 $ gen 0 127
  property "Int16" . tryFromTryFrom @Int.Int16 $ gen 0 32767
  property "Int32" . tryFromTryFrom @Int.Int32 $ gen 0 2147483647
  property "Int64" . tryFromTryFrom @Int.Int64 $ gen 0 9223372036854775807
  property "Int" . tryFromTryFrom @Int $ gen 0 9223372036854775807
  property "Integer" . fromTryFrom @Integer $ gen 0 18446744073709551615
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word32" . tryFromTryFrom @Word.Word32 $ gen 0 4294967295
  property "Word64" . fromTryFrom @Word.Word64 $ gen 0 18446744073709551615
  property "Natural" . fromTryFrom @Natural.Natural $ gen 0 18446744073709551615
  property "Float" . tryFromTryFrom @Float $ gen 0 16777215
  property "Double" . tryFromTryFrom @Double $ gen 0 9007199254740991

groupNatural :: H.Group
groupNatural = group "Natural" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Natural.Natural
  property "Int8" . tryFromTryFrom @Int.Int8 $ gen 0 127
  property "Int16" . tryFromTryFrom @Int.Int16 $ gen 0 32767
  property "Int32" . tryFromTryFrom @Int.Int32 $ gen 0 2147483647
  property "Int64" . tryFromTryFrom @Int.Int64 $ gen 0 9223372036854775807
  property "Int" . tryFromTryFrom @Int $ gen 0 9223372036854775807
  property "Integer" . fromTryFrom @Integer $ gen 0 99999999999999999999
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ gen 0 65535
  property "Word32" . tryFromFrom @Word.Word32 $ gen 0 4294967295
  property "Word64" . tryFromFrom @Word.Word64 $ gen 0 18446744073709551615
  property "Word" . tryFromFrom @Word $ gen 0 18446744073709551615
  property "Float" . tryFromTryFrom @Float $ gen 0 16777215
  property "Double" . tryFromTryFrom @Double $ gen 0 9007199254740991

groupFloat :: H.Group
groupFloat = group "Float" $ do
  let genI lo hi = fmap (fromIntegral @Int.Int32) . Gen.integral $ Range.linear lo hi :: H.Gen Float
  property "Int8" . tryFromFrom @Int.Int8 $ genI -128 127
  property "Int16" . tryFromFrom @Int.Int16 $ genI -32768 32767
  property "Int32" . tryFromTryFrom @Int.Int32 $ genI -16777215 16777215
  property "Int64" . tryFromTryFrom @Int.Int64 $ genI -16777215 16777215
  property "Int" . tryFromTryFrom @Int $ genI -16777215 16777215
  property "Integer" . tryFromTryFrom @Integer $ genI -16777215 16777215
  property "Word8" . tryFromFrom @Word.Word8 $ genI 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ genI 0 65535
  property "Word32" . tryFromTryFrom @Word.Word32 $ genI 0 16777215
  property "Word64" . tryFromTryFrom @Word.Word64 $ genI 0 16777215
  property "Word" . tryFromTryFrom @Word $ genI 0 16777215
  property "Natural" . tryFromTryFrom @Natural.Natural $ genI 0 16777215
  let genF lo hi = Gen.realFloat $ Range.linearFrac lo hi :: H.Gen Float
  property "Rational" . tryFromFrom @Rational $ genF -16777215 16777215
  property "Double" . fromFrom @Double $ genF -16777215 16777215

groupDouble :: H.Group
groupDouble = group "Double" $ do
  let genI lo hi = fmap (fromIntegral @Int.Int64) . Gen.integral $ Range.linear lo hi :: H.Gen Double
  property "Int8" . tryFromFrom @Int.Int8 $ genI -128 127
  property "Int16" . tryFromFrom @Int.Int16 $ genI -32768 32767
  property "Int32" . tryFromFrom @Int.Int32 $ genI -2147483648 2147483647
  property "Int64" . tryFromTryFrom @Int.Int64 $ genI -9007199254740991 9007199254740991
  property "Int" . tryFromTryFrom @Int $ genI -9007199254740991 9007199254740991
  property "Integer" . tryFromTryFrom @Integer $ genI -9007199254740991 9007199254740991
  property "Word8" . tryFromFrom @Word.Word8 $ genI 0 255
  property "Word16" . tryFromFrom @Word.Word16 $ genI 0 65535
  property "Word32" . tryFromFrom @Word.Word32 $ genI 0 4294967295
  property "Word64" . tryFromTryFrom @Word.Word64 $ genI 0 9007199254740991
  property "Word" . tryFromTryFrom @Word $ genI 0 9007199254740991
  property "Natural" . tryFromTryFrom @Natural.Natural $ genI 0 9007199254740991
  let genF lo hi = Gen.realFloat $ Range.linearFrac lo hi :: H.Gen Double
  property "Rational" . tryFromFrom @Rational $ genF -9007199254740991 9007199254740991
  property "Float" . fromFrom @Float $ genF -16777215 16777215

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
  unsafeFromInteger $
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
  unsafeFromInteger $
    min
      (toInteger $ asProxy a maxBound)
      (toInteger $ asProxy b maxBound)

asProxy :: proxy a -> a -> a
asProxy = const id

p :: Typeable.Proxy a
p = Typeable.Proxy

unsafeFromInteger ::
  (Stack.HasCallStack, Bits.Bits a, Integral a) =>
  Integer ->
  a
unsafeFromInteger = Maybe.fromJust . Bits.toIntegralSized

group ::
  H.GroupName ->
  Writer.Writer [(H.PropertyName, H.Property)] () ->
  H.Group
group n = H.Group n . Writer.execWriter

property ::
  (Monad m) =>
  H.PropertyName ->
  H.PropertyT IO () ->
  Writer.WriterT [(H.PropertyName, H.Property)] m ()
property n = Writer.tell . pure . (,) n . H.property

fromFrom ::
  forall target source m.
  ( Eq source,
    Eq target,
    Witch.From source target,
    Witch.From target source,
    Monad m,
    Show source,
    Show target
  ) =>
  H.Gen source ->
  H.PropertyT m ()
fromFrom = fromFromP (p @source) (p @target)

fromFromP ::
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
fromFromP s t =
  tripping
    (right . Witch.from . asProxy s)
    (right . Witch.from . asProxy t)

fromTryFrom ::
  forall target source m.
  ( Eq source,
    Eq target,
    Witch.From source target,
    Witch.TryFrom target source,
    Monad m,
    Show source,
    Show target,
    Typeable.Typeable source,
    Typeable.Typeable target
  ) =>
  H.Gen source ->
  H.PropertyT m ()
fromTryFrom = fromTryFromP (p @source) (p @target)

fromTryFromP ::
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
fromTryFromP s t =
  tripping
    (right . Witch.from . asProxy s)
    (Witch.tryFrom . asProxy t)

tryFromFrom ::
  forall target source m.
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
  H.Gen source ->
  H.PropertyT m ()
tryFromFrom = tryFromFromP (p @source) (p @target)

tryFromFromP ::
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
tryFromFromP s t =
  tripping
    (Witch.tryFrom . asProxy s)
    (right . Witch.from . asProxy t)

tryFromTryFrom ::
  forall target source m.
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
  H.Gen source ->
  H.PropertyT m ()
tryFromTryFrom = tryFromTryFromP (p @source) (p @target)

tryFromTryFromP ::
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
tryFromTryFromP s t =
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
