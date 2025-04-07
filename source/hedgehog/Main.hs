{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
  [ witchGroup,
    int8Group,
    int16Group,
    int32Group,
    int64Group,
    intGroup,
    integerGroup
  ]

witchGroup :: H.Group
witchGroup =
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

int8Group :: H.Group
int8Group = group "Int8" $ do
  let g = Gen.integral Range.exponentialBounded :: H.Gen Int.Int8
  property "Int16" $ fromTryFrom @Int.Int16 g
  property "Int32" $ fromTryFrom @Int.Int32 g
  property "Int64" $ fromTryFrom @Int.Int64 g
  property "Int" $ fromTryFrom @Int g
  property "Integer" $ fromTryFrom @Integer g
  property "Word8" $ tryFromTryFrom @Word.Word8 g
  property "Word16" $ tryFromTryFrom @Word.Word16 g
  property "Word32" $ tryFromTryFrom @Word.Word32 g
  property "Word64" $ tryFromTryFrom @Word.Word64 g
  property "Word" $ tryFromTryFrom @Word g
  property "Natural" $ tryFromTryFrom @Natural.Natural g
  property "Float" $ fromTryFrom @Float g
  property "Double" $ fromTryFrom @Double g

int16Group :: H.Group
int16Group = group "Int16" $ do
  let g = Gen.integral Range.exponentialBounded :: H.Gen Int.Int16
  property "Int8" $ tryFromFrom @Int.Int8 g
  property "Int32" $ fromTryFrom @Int.Int32 g
  property "Int64" $ fromTryFrom @Int.Int64 g
  property "Int" $ fromTryFrom @Int g
  property "Integer" $ fromTryFrom @Integer g
  property "Word8" $ tryFromFrom @Word.Word8 g
  property "Word16" $ tryFromTryFrom @Word.Word16 g
  property "Word32" $ tryFromTryFrom @Word.Word32 g
  property "Word64" $ tryFromTryFrom @Word.Word64 g
  property "Word" $ tryFromTryFrom @Word g
  property "Natural" $ tryFromTryFrom @Natural.Natural g
  property "Float" $ fromTryFrom @Float g
  property "Double" $ fromTryFrom @Double g

int32Group :: H.Group
int32Group = group "Int32" $ do
  let g = Gen.integral Range.exponentialBounded :: H.Gen Int.Int32
  property "Int8" $ tryFromFrom @Int.Int8 g
  property "Int16" $ tryFromFrom @Int.Int16 g
  property "Int64" $ fromTryFrom @Int.Int64 g
  property "Int" $ tryFromTryFrom @Int g
  property "Integer" $ fromTryFrom @Integer g
  property "Word8" $ tryFromFrom @Word.Word8 g
  property "Word16" $ tryFromFrom @Word.Word16 g
  property "Word32" $ tryFromTryFrom @Word.Word32 g
  property "Word64" $ tryFromTryFrom @Word.Word64 g
  property "Word" $ tryFromTryFrom @Word g
  property "Natural" $ tryFromTryFrom @Natural.Natural g
  property "Float" $ tryFromTryFrom @Float g
  property "Double" $ fromTryFrom @Double g

int64Group :: H.Group
int64Group = group "Int64" $ do
  let g = Gen.integral Range.exponentialBounded :: H.Gen Int.Int64
  property "Int8" $ tryFromFrom @Int.Int8 g
  property "Int16" $ tryFromFrom @Int.Int16 g
  property "Int32" $ tryFromFrom @Int.Int32 g
  property "Int" $ tryFromFrom @Int g
  property "Integer" $ fromTryFrom @Integer g
  property "Word8" $ tryFromFrom @Word.Word8 g
  property "Word16" $ tryFromFrom @Word.Word16 g
  property "Word32" $ tryFromFrom @Word.Word32 g
  property "Word64" $ tryFromTryFrom @Word.Word64 g
  property "Word" $ tryFromTryFrom @Word g
  property "Natural" $ tryFromTryFrom @Natural.Natural g
  property "Float" $ tryFromTryFrom @Float g
  property "Double" $ tryFromTryFrom @Double g

intGroup :: H.Group
intGroup = group "Int" $ do
  let g = Gen.integral Range.exponentialBounded :: H.Gen Int
  property "Int8" $ tryFromFrom @Int.Int8 g
  property "Int16" $ tryFromFrom @Int.Int16 g
  property "Int32" $ tryFromTryFrom @Int.Int32 g
  property "Int64" $ fromTryFrom @Int.Int64 g
  property "Integer" $ fromTryFrom @Integer g
  property "Word8" $ tryFromFrom @Word.Word8 g
  property "Word16" $ tryFromFrom @Word.Word16 g
  property "Word32" $ tryFromTryFrom @Word.Word32 g
  property "Word64" $ tryFromTryFrom @Word.Word64 g
  property "Word" $ tryFromTryFrom @Word g
  property "Natural" $ tryFromTryFrom @Natural.Natural g
  property "Float" $ tryFromTryFrom @Float g
  property "Double" $ tryFromTryFrom @Double g

integerGroup :: H.Group
integerGroup = group "Integer" $ do
  let lo = toInteger (minBound :: Int.Int64)
      hi = toInteger (maxBound :: Int.Int64)
      g = Gen.integral $ Range.exponential lo hi :: H.Gen Integer
  property "Int8" $ tryFromFrom @Int.Int8 g
  property "Int16" $ tryFromFrom @Int.Int16 g
  property "Int32" $ tryFromFrom @Int.Int32 g
  property "Int64" $ tryFromFrom @Int.Int64 g
  property "Int" $ tryFromFrom @Int g
  property "Word8" $ tryFromFrom @Word.Word8 g
  property "Word16" $ tryFromFrom @Word.Word16 g
  property "Word32" $ tryFromFrom @Word.Word32 g
  property "Word64" $ tryFromFrom @Word.Word64 g
  property "Word" $ tryFromFrom @Word g
  property "Natural" $ tryFromFrom @Natural.Natural g
  property "Float" $ tryFromTryFrom @Float g
  property "Double" $ tryFromTryFrom @Double g

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
