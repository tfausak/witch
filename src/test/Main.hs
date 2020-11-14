{-# language TypeApplications #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Tuple as Tuple
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.QuickCheck as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

main :: IO ()
main = Hspec.hspec . Hspec.parallel $ do

  Hspec.describe "From a a" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int @Int x == x
    Hspec.prop "into" $ \ x -> Witch.into @Int @Int x == x
    Hspec.prop "via" $ \ x -> Witch.via @Int @Int @Int x == x

  Hspec.prop "From a (x -> a)" $ \ x ->
    Witch.from @Int @(() -> Int) x () == x

  Hspec.prop "From a [a]" $ \ x ->
    Witch.from @Int @[Int] x == [x]

  Hspec.prop "From a (Maybe a)" $ \ x ->
    Witch.from @Int @(Maybe Int) x == Just x

  Hspec.prop "From a (Either a x)" $ \ x ->
    Witch.from @Int @(Either Int ()) x == Left x

  Hspec.prop "From a (Either x a)" $ \ x ->
    Witch.from @Int @(Either () Int) x == Right x

  Hspec.it "From Void x" $ Hspec.pending

  Hspec.prop "From (a, x) a" $ \ x ->
    Witch.from @(Int, ()) @Int (x, ()) == x

  Hspec.prop "From (x, a) a" $ \ x ->
    Witch.from @((), Int) @Int ((), x) == x

  Hspec.prop "From (a, b) (b, a)" $ \ x ->
    Witch.from @(Char, Int) @(Int, Char) x == Tuple.swap x

  Hspec.prop "From (NonEmpty a) [a]" $ \ x ->
    Witch.from @(NonEmpty.NonEmpty Int) @[Int] x == NonEmpty.toList x

  Hspec.prop "From Word8 Word16" $ \ x ->
    Witch.from @Word.Word8 @Word.Word16 x == fromIntegral x

  Hspec.prop "From Word16 Word32" $ \ x ->
    Witch.from @Word.Word16 @Word.Word32 x == fromIntegral x

  Hspec.prop "From Word32 Word64" $ \ x ->
    Witch.from @Word.Word32 @Word.Word64 x == fromIntegral x

  Hspec.prop "From Word Natural" $ \ x ->
    Witch.from @Word @Natural.Natural x == fromIntegral x

  Hspec.prop "From Natural Integer" $ \ x ->
    Witch.from @Natural.Natural @Integer x == fromIntegral x

  Hspec.prop "From Int8 Int16" $ \ x ->
    Witch.from @Int.Int8 @Int.Int16 x == fromIntegral x

  Hspec.prop "From Int16 Int32" $ \ x ->
    Witch.from @Int.Int16 @Int.Int32 x == fromIntegral x

  Hspec.prop "From Int32 Int64" $ \ x ->
    Witch.from @Int.Int32 @Int.Int64 x == fromIntegral x

  Hspec.prop "From Int Integer" $ \ x ->
    Witch.from @Int @Integer x == fromIntegral x

  Hspec.prop "From Integer Rational" $ \ x ->
    Witch.from @Integer @Rational x == fromIntegral x

  Hspec.prop "From Float Double" $ \ x ->
    Witch.from @Float @Double x == realToFrac x

  Hspec.prop "From Bool Int" $ \ x ->
    Witch.from @Bool @Int x == fromEnum x

  Hspec.prop "From Char Int" $ \ x ->
    Witch.from @Char @Int x == fromEnum x

  Hspec.prop "From [Word8] ByteString" $ \ x ->
    Witch.from @[Word.Word8] @B.ByteString x == B.pack x

  Hspec.prop "From ByteString [Word8]" $ \ x ->
    Witch.from @B.ByteString @[Word.Word8] x == B.unpack x

  Hspec.prop "From ByteString LazyByteString" $ \ x ->
    Witch.from @B.ByteString @LB.ByteString x == LB.fromStrict x

  Hspec.prop "From LazyByteString ByteString" $ \ x ->
    Witch.from @LB.ByteString @B.ByteString x == LB.toStrict x

  Hspec.prop "From String Text" $ \ x ->
    Witch.from @String @T.Text x == T.pack x

  Hspec.prop "From Text String" $ \ x ->
    Witch.from @T.Text @String x == T.unpack x

  Hspec.prop "From Text LazyText" $ \ x ->
    Witch.from @T.Text @LT.Text x == LT.fromStrict x

  Hspec.prop "From LazyText Text" $ \ x ->
    Witch.from @LT.Text @T.Text x == LT.toStrict x

  Hspec.prop "From [a] (Seq a)" $ \ x ->
    Witch.from @[Int] @(Seq.Seq Int) x == Seq.fromList x

  Hspec.prop "From (Seq a) [a]" $ \ x ->
    Witch.from @(Seq.Seq Int) @[Int] x == Foldable.toList x

  Hspec.prop "From [a] (Set a)" $ \ x ->
    Witch.from @[Int] @(Set.Set Int) x == Set.fromList x

  Hspec.prop "From (Set a) [a]" $ \ x ->
    Witch.from @(Set.Set Int) @[Int] x == Set.toAscList x

  Hspec.prop "From [(k, v)] (Map k v)" $ \ x ->
    Witch.from @[(Char, Int)] @(Map.Map Char Int) x == Map.fromList x

  Hspec.prop "From (Map k v) [(k, v)]" $ \ x ->
    Witch.from @(Map.Map Char Int) @[(Char, Int)] x == Map.toAscList x

instance QuickCheck.Arbitrary Natural.Natural where
  arbitrary = QuickCheck.arbitrarySizedNatural
  shrink = QuickCheck.shrinkIntegral

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (NonEmpty.NonEmpty a) where
  arbitrary = QuickCheck.applyArbitrary2 (NonEmpty.:|)
  shrink = QuickCheck.genericShrink

instance QuickCheck.Arbitrary LB.ByteString where
  arbitrary = fmap LB.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap LB.pack LB.unpack

instance QuickCheck.Arbitrary B.ByteString where
  arbitrary = fmap B.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap B.pack B.unpack

instance QuickCheck.Arbitrary T.Text where
  arbitrary = fmap T.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap T.pack T.unpack

instance QuickCheck.Arbitrary LT.Text where
  arbitrary = fmap LT.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap LT.pack LT.unpack
