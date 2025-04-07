{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Int as Int
import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import qualified Data.Void as Void
import qualified Data.Word as Word
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
    groupInteger
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
        $ Gen.word8 Range.exponentialBounded,
      (,) "tripping Word8 Word16"
        . H.property
        . fromTryFrom @Word.Word16
        $ Gen.word8 Range.exponentialBounded,
      (,) "tripping Word16 Word8"
        . H.property
        . tryFromFrom @Word.Word8
        $ Gen.word16 Range.exponentialBounded,
      (,) "tripping Word Word32"
        . H.property
        . tryFromTryFrom @Word.Word32
        $ Gen.word Range.exponentialBounded
    ]

groupInt8 :: H.Group
groupInt8 = group "Int8" $ do
  let gen lo hi = Gen.integral $ Range.linear lo hi :: H.Gen Int.Int8
  property "Int16" . fromTryFrom @Int.Int16 $ gen -128 127
  property "Int32" . fromTryFrom @Int.Int32 $ gen -128 127
  property "Int64" . fromTryFrom @Int.Int64 $ gen -128 127
  property "Int" . fromTryFrom @Int $ gen -128 127
  property "Integer" . fromTryFrom @Integer $ gen -128 127
  property "Word8" . tryFromTryFrom @Word.Word8 $ gen 0 127
  property "Word16" . tryFromTryFrom @Word.Word16 $ gen 0 127
  property "Word32" . tryFromTryFrom @Word.Word32 $ gen 0 127
  property "Word64" . tryFromTryFrom @Word.Word64 $ gen 0 127
  property "Word" . tryFromTryFrom @Word $ gen 0 127
  property "Natural" . tryFromTryFrom @Natural.Natural $ gen 0 127
  property "Float" . fromTryFrom @Float $ gen -128 127
  property "Double" . fromTryFrom @Double $ gen -128 127

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
