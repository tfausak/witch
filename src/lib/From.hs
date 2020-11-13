{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}

module From (From(from), into, via) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Coerce as Coerce
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Tuple as Tuple
import qualified Data.Void as Void
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural

class From a b where
  from :: a -> b
  default from :: Coerce.Coercible a b => a -> b
  from = Coerce.coerce

into :: forall b a . From a b => a -> b
into = from

via :: forall b a c . (From a b, From b c) => a -> c
via = from . (\ x -> x :: b) . from

instance From (a, b) (b, a) where from = Tuple.swap
instance From (a, x) a where from = fst
instance From (Map.Map k v) [(k, v)] where from = Map.assocs
instance From (NonEmpty.NonEmpty a) [a] where from = NonEmpty.toList
instance From (Seq.Seq a) [a] where from = Foldable.toList
instance From (Set.Set a) [a] where from = Set.toList
instance From (x, a) a where from = snd
instance From [a] (Seq.Seq a) where from = Seq.fromList
instance From [Word.Word8] ByteString.ByteString where from = ByteString.pack
instance From a (Either a x) where from = Left
instance From a (Either x a) where from = Right
instance From a (Maybe a) where from = Just
instance From a (x -> a) where from = const
instance From a [a] where from = pure
instance From a a where from = id
instance From Bool Int where from = fromEnum
instance From ByteString.ByteString [Word.Word8] where from = ByteString.unpack
instance From ByteString.ByteString LazyByteString.ByteString where from = LazyByteString.fromStrict
instance From Float Double where from = realToFrac
instance From Int Integer where from = fromIntegral
instance From Int.Int16 Int where from = fromIntegral
instance From Int.Int16 Int.Int32 where from = fromIntegral
instance From Int.Int32 Int.Int64 where from = fromIntegral
instance From Int.Int8 Int where from = fromIntegral
instance From Int.Int8 Int.Int16 where from = fromIntegral
instance From Integer Rational where from = fromIntegral
instance From LazyByteString.ByteString ByteString.ByteString where from = LazyByteString.toStrict
instance From LazyText.Text Text.Text where from = LazyText.toStrict
instance From Natural.Natural Integer where from = fromIntegral
instance From String Text.Text where from = Text.pack
instance From Text.Text LazyText.Text where from = LazyText.fromStrict
instance From Text.Text String where from = Text.unpack
instance From Void.Void a where from = Void.absurd
instance From Word Integer where from = fromIntegral
instance From Word Rational where from = fromIntegral
instance From Word.Word16 Word where from = fromIntegral
instance From Word.Word16 Word.Word32 where from = fromIntegral
instance From Word.Word32 Word.Word64 where from = fromIntegral
instance From Word.Word8 Word where from = fromIntegral
instance From Word.Word8 Word.Word16 where from = fromIntegral
instance Ord a => From [a] (Set.Set a) where from = Set.fromList
instance Ord k => From [(k, v)] (Map.Map k v) where from = Map.fromList
