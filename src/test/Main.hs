{-# language TypeApplications #-}

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
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

  Hspec.describe "From a (x -> a)" $ pure ()

  Hspec.describe "From a [a]" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int @[Int] x == [x]

  Hspec.describe "From a (Maybe a)" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int @(Maybe Int) x == Just x

  Hspec.describe "From a (Either a x)" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int @(Either Int ()) x == Left x

  Hspec.describe "From a (Either x a)" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int @(Either () Int) x == Right x

  Hspec.describe "From Void x" $ pure ()

  Hspec.describe "From (a, x) a" $ do
    Hspec.prop "from" $ \ x -> Witch.from @(Int, ()) @Int (x, ()) == x

  Hspec.describe "From (x, a) a" $ do
    Hspec.prop "from" $ \ x -> Witch.from @((), Int) @Int ((), x) == x

  Hspec.describe "From (a, b) (b, a)" $ do
    Hspec.prop "from" $ \ x y -> Witch.from @(Char, Int) @(Int, Char) (x, y) == (y, x)

  Hspec.describe "From (NonEmpty a) [a]" $ do
    Hspec.prop "from" $ \ x -> Witch.from @(NonEmpty.NonEmpty Int) @[Int] x == NonEmpty.toList x

  Hspec.describe "From Word8 Word16" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Word.Word8 @Word.Word16 x == fromIntegral x

  Hspec.describe "From Word16 Word32" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Word.Word16 @Word.Word32 x == fromIntegral x

  Hspec.describe "From Word32 Word64" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Word.Word32 @Word.Word64 x == fromIntegral x

  Hspec.describe "From Word Natural" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Word @Natural.Natural x == fromIntegral x

  Hspec.describe "From Natural Integer" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Natural.Natural @Integer x == fromIntegral x

  Hspec.describe "From Int8 Int16" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int.Int8 @Int.Int16 x == fromIntegral x

  Hspec.describe "From Int16 Int32" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int.Int16 @Int.Int32 x == fromIntegral x

  Hspec.describe "From Int32 Int64" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int.Int32 @Int.Int64 x == fromIntegral x

  Hspec.describe "From Int Integer" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Int @Integer x == fromIntegral x

  Hspec.describe "From Integer Rational" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Integer @Rational x == fromIntegral x

  Hspec.describe "From Float Double" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Float @Double x == realToFrac x

  Hspec.describe "From Bool Int" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Bool @Int x == fromEnum x

  Hspec.describe "From Char Int" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Char @Int x == fromEnum x

  Hspec.describe "From [Word8] ByteString" $ do
    Hspec.prop "from" $ \ x -> Witch.from @[Word.Word8] @ByteString.ByteString x == ByteString.pack x

  Hspec.describe "From ByteString [Word8]" $ do
    Hspec.prop "from" $ \ x -> Witch.from @ByteString.ByteString @[Word.Word8] x == ByteString.unpack x

  Hspec.describe "From ByteString LazyByteString" $ do
    Hspec.prop "from" $ \ x -> Witch.from @ByteString.ByteString @LazyByteString.ByteString x == LazyByteString.fromStrict x

  Hspec.describe "From LazyByteString ByteString" $ do
    Hspec.prop "from" $ \ x -> Witch.from @LazyByteString.ByteString @ByteString.ByteString x == LazyByteString.toStrict x

  Hspec.describe "From String Text" $ do
    Hspec.prop "from" $ \ x -> Witch.from @String @Text.Text x == Text.pack x

  Hspec.describe "From Text String" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Text.Text @String x == Text.unpack x

  Hspec.describe "From Text LazyText" $ do
    Hspec.prop "from" $ \ x -> Witch.from @Text.Text @LazyText.Text x == LazyText.fromStrict x

  Hspec.describe "From LazyText Text" $ do
    Hspec.prop "from" $ \ x -> Witch.from @LazyText.Text @Text.Text x == LazyText.toStrict x

  Hspec.describe "From [a] (Seq a)" $ do
    Hspec.prop "from" $ \ x -> Witch.from @[Int] @(Seq.Seq Int) x == Seq.fromList x

  Hspec.describe "From (Seq a) [a]" $ do
    Hspec.prop "from" $ \ x -> Witch.from @(Seq.Seq Int) @[Int] x == Foldable.toList x

  Hspec.describe "From [a] (Set a)" $ do
    Hspec.prop "from" $ \ x -> Witch.from @[Int] @(Set.Set Int) x == Set.fromList x

  Hspec.describe "From (Set a) [a]" $ do
    Hspec.prop "from" $ \ x -> Witch.from @(Set.Set Int) @[Int] x == Set.toAscList x

  Hspec.describe "From [(k, v)] (Map k v)" $ do
    Hspec.prop "from" $ \ x -> Witch.from @[(Char, Int)] @(Map.Map Char Int) x == Map.fromList x

  Hspec.describe "From (Map k v) [(k, v)]" $ do
    Hspec.prop "from" $ \ x -> Witch.from @(Map.Map Char Int) @[(Char, Int)] x == Map.toAscList x

instance QuickCheck.Arbitrary Natural.Natural where
  arbitrary = QuickCheck.arbitrarySizedNatural
  shrink = QuickCheck.shrinkIntegral

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (NonEmpty.NonEmpty a) where
  arbitrary = QuickCheck.applyArbitrary2 (NonEmpty.:|)
  shrink = QuickCheck.genericShrink

instance QuickCheck.Arbitrary LazyByteString.ByteString where
  arbitrary = fmap LazyByteString.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap LazyByteString.pack LazyByteString.unpack

instance QuickCheck.Arbitrary ByteString.ByteString where
  arbitrary = fmap ByteString.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap ByteString.pack ByteString.unpack

instance QuickCheck.Arbitrary Text.Text where
  arbitrary = fmap Text.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap Text.pack Text.unpack

instance QuickCheck.Arbitrary LazyText.Text where
  arbitrary = fmap LazyText.pack QuickCheck.arbitrary
  shrink = QuickCheck.shrinkMap LazyText.pack LazyText.unpack
