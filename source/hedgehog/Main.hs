{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Bits as Bits
import qualified Data.Int as Int
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
groups = Writer.execWriter $ do
  Writer.tell $ pure groupWitch
  Writer.tell $ pure groupInt8
  Writer.tell $ pure groupInt16
  Writer.tell $ pure groupInt32
  Writer.tell $ pure groupInt64
  Writer.tell $ pure groupInt
  Writer.tell $ pure groupInteger
  Writer.tell $ pure groupWord8
  Writer.tell $ pure groupWord16
  Writer.tell $ pure groupWord32
  Writer.tell $ pure groupWord64
  Writer.tell $ pure groupWord
  Writer.tell $ pure groupNatural
  Writer.tell $ pure groupFloat
  Writer.tell $ pure groupDouble

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
  property "Int16" . fromTryFrom @Int.Int16 . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Int.Int16
  property "Int32" . fromTryFrom @Int.Int32 . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Int.Int32
  property "Int64" . fromTryFrom @Int.Int64 . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Int.Int64
  property "Int" . fromTryFrom @Int . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Int
  property "Integer" . fromTryFrom @Integer . Gen.integral $ Range.linear @Int.Int8 -128 127
  property "Word8" . tryFromTryFrom @Word.Word8 . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Word.Word8
  property "Word16" . tryFromTryFrom @Word.Word16 . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Word.Word16
  property "Word32" . tryFromTryFrom @Word.Word32 . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Word.Word32
  property "Word64" . tryFromTryFrom @Word.Word64 . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Word.Word64
  property "Word" . tryFromTryFrom @Word . Gen.integral . uncurry Range.linear $ mkBounds @Int.Int8 @Word
  property "Natural" . tryFromTryFrom @Natural.Natural . Gen.integral $ Range.linear @Int.Int8 0 127
  property "Float" . fromTryFrom @Float . Gen.integral $ Range.linear @Int.Int8 -128 127
  property "Double" . fromTryFrom @Double . Gen.integral $ Range.linear @Int.Int8 -128 127

groupInt16 :: H.Group
groupInt16 = group "Int16" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Int.Int16
  property "Int8" . tryFromFrom @Int.Int8 $ gen -128 127
  property "Int32" . fromTryFrom @Int.Int32 $ gen -32768 32767
  property "Int64" . fromTryFrom @Int.Int64 $ gen -32768 32767
  property "Int" . fromTryFrom @Int $ gen -32768 32767
  property "Integer" . fromTryFrom @Integer $ gen -32768 32767
  property "Word8" . tryFromFrom @Word.Word8 $ gen 0 255
  property "Word16" . tryFromTryFrom @Word.Word16 $ gen 0 32767
  property "Word32" . tryFromTryFrom @Word.Word32 $ gen 0 32767
  property "Word64" . tryFromTryFrom @Word.Word64 $ gen 0 32767
  property "Word" . tryFromTryFrom @Word $ gen 0 32767
  property "Natural" . tryFromTryFrom @Natural.Natural $ gen 0 32767
  property "Float" . fromTryFrom @Float $ gen -32768 32767
  property "Double" . fromTryFrom @Double $ gen -32768 32767

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

mkBounds ::
  forall a b.
  ( Stack.HasCallStack,
    Bits.Bits a,
    Bounded a,
    Bounded b,
    Integral a,
    Integral b,
    Typeable.Typeable a
  ) =>
  (a, a)
mkBounds =
  ( unsafeFromInteger $ max (toInteger $ minBound @a) (toInteger $ minBound @b),
    unsafeFromInteger $ min (toInteger $ maxBound @a) (toInteger $ maxBound @b)
  )

unsafeFromInteger ::
  forall a.
  ( Stack.HasCallStack,
    Bits.Bits a,
    Integral a,
    Typeable.Typeable a
  ) =>
  Integer ->
  a
unsafeFromInteger integer = case Bits.toIntegralSized integer of
  Nothing ->
    error $
      ( showString "unsafeFromInteger @"
          . showsPrec 11 (Typeable.typeRep (Typeable.Proxy :: Typeable.Proxy a))
          . showString " "
          . shows integer
      )
        ""
  Just x -> x

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

-- | Tests round-tripping between two types using 'Witch.From' in both
-- directions.
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
fromFrom = tripping (right . Witch.from) $ right . Witch.from @target

-- | Tests round-tripping between two types using 'Witch.From' in one
-- direction and 'Witch.TryFrom' in the other.
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
fromTryFrom = tripping (right . Witch.from) $ Witch.tryFrom @target

-- | Tests round-tripping between two types using 'Witch.TryFrom' in one
-- direction and 'Witch.From' in the other.
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
tryFromFrom = tripping Witch.tryFrom $ right . Witch.from @target

-- | Tests round-tripping between two types using 'Witch.TryFrom' in both
-- directions.
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
tryFromTryFrom = tripping Witch.tryFrom $ Witch.tryFrom @target

-- | Generic function for testing round-tripping between types. Consider using
-- one of these more specific functions instead:
--
-- - 'fromFrom'
-- - 'fromTryFrom'
-- - 'tryFromFrom'
-- - 'tryFromTryFrom'
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

-- | Just used to pick a concrete type for the 'Left' case.
right :: a -> Either Void.Void a
right = Right
