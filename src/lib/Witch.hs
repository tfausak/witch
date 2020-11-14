{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}

-- | This module provides the 'From' type class for converting values between
-- various types. This aims to be a common interface for the various @xToY@ or
-- @yFromX@ functions you might write instead. It is inspired by the
-- @std::convert::From@ trait that the Rust programming language provides.
--
-- Many Haskell libraries already provide similar functionality. Here's how
-- this module compares to them:
--
-- - <https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Coerce.html>:
--   This type class is convenient because it's automatically inferred by the
--   compiler, but it only works for types that have the same runtime
--   representation.
--
-- - <https://hackage.haskell.org/package/convertible-1.1.1.0/docs/Data-Convertible-Base.html>:
--   This type class allows for conversions to fail.
--
-- - <https://hackage.haskell.org/package/basement-0.0.11/docs/Basement-From.html>:
--   This type class is essentially the same, but the @basement@ package is an
--   alternative standard library that some people may not want to depend on.
--
-- - <https://hackage.haskell.org/package/inj-base-0.2.0.0/docs/Inj-Base.html>:
--   This type class requires conversions to be injective, as opposed to merely
--   suggesting it. Also some conversions fail at runtime.
--
-- - <https://github.com/mbj/conversions/blob/6ac6c52/src/Data/Conversions/FromType.hs>:
--   This type class comes with many convenient helper functions, but some of
--   the provided instances fail at runtime.
--
-- - <https://github.com/kframework/kore/blob/626f230/kore/src/From.hs>:
--   This package is not available on Hackage, but otherwise is very similar to
--   this one.
module Witch (From(from), into, via) where

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

-- | This type class represents a way to convert values from some type into
-- another type. The constraint @From a b@ means that you can convert from a
-- value of type @a@ into a value of type @b@.
--
-- This is primarily intended for "zero cost" conversions like @newtype@s. For
-- example if you wanted to have a type to represent someone's name, you could
-- say:
--
-- > newtype Name = Name String
-- > instance From String Name
-- > instance From Name String
--
-- And then you could convert back and forth between @Name@s and @String@s:
--
-- > let someString = "Taylor"
-- > let someName = Name someString
-- > into @Name someString -- convert from string to name
-- > into @String someName -- convert from name to string
--
-- This type class does not have any laws, but it does have some expectations:
--
-- - Conversions should be total. A conversion should never fail or crash.
--   Avoid writing instances like @From Int (Maybe Char)@. (It might be
--   worthwhile to have a separate @TryFrom@ type class for this.)
--
-- - Conversions should be unambiguous. For example there are many ways to
--   decode a @ByteString@ into @Text@, so you shouldn't provide an instance
--   for that.
--
-- - Conversions should be cheap, ideally free. For example converting from
--   @String@ to @Text@ is probably fine, but converting from a UTF-8 encoded
--   @ByteString@ to @Text@ is problematic.
--
-- - Conversions should be lossless. In other words if you have @From a b@ then
--   no two @a@ values should be converted to the same @b@ value. For example
--   @From Int Integer@ is fine because every @Int@ can be mapped to a
--   corresponding @Integer@, but @From Integer Int@ is not good because some
--   @Integer@s are out of bounds and would need to be clamped.
--
-- - If you have both @From a b@ and @From b a@, then @from . from@ should be
--   the same as 'id'. In other words @a@ and @b@ are isomorphic.
--
-- - If you have both @From a b@ and @From b c@, then it's up to you if you
--   want to provide @From a c@. Sometimes using 'via' is ergonomic enough,
--   other times you want the extra instance. (It would be nice if we could
--   provide @instance (From a b, From b c) => From a c where from = via \@b@.)
class From source target where
  -- | This method converts a value from one type into another. This is
  -- intended to be used with the @TypeApplications@ language extension. For
  -- example, here are a few ways to convert from an 'Int' into an 'Integer':
  --
  -- > from @Int @Integer 123
  -- > from @_ @Integer (123 :: Int)
  -- > from @Int @_ 123 :: Integer
  -- > from @Int 123 :: Integer
  -- > from (123 :: Int) :: Integer
  --
  -- Often the context around an expression will make the explicit type
  -- signatures unnecessary. If you find yourself using a partial type
  -- signature, consider using 'into' instead. For example:
  --
  -- > let someInt = 123 :: Int
  -- > from @_ @Integer someInt -- avoid this
  -- > into @Integer someInt -- prefer this
  --
  -- The default implementation of 'from' simply calls 'Coerce.coerce', which
  -- works for types that have the same runtime representation.
  from :: source -> target
  default from :: Coerce.Coercible source target => source -> target
  from = Coerce.coerce

-- | This function converts a value from one type into another. This is the
-- same as 'from' except that the type variables are in the opposite order.
into :: forall target source . From source target => source -> target
into = from

-- | This function converts a value from one type into another by going through
-- some third type. This is the same as calling 'from' (or 'into') twice, but
-- can sometimes be more convenient.
--
-- Note that the type in the middle of the conversion is the first type
-- variable of this function. In other words, @via \@b \@a \@c@ first converts
-- from @a@ to @b@, and then from @b@ to @c@. Often both @a@ and @c@ will be
-- inferred from context, which means you can just write @via \@b@.
via :: forall through source target . (From source through, From through target) => source -> target
via = from . (\ x -> x :: through) . from

-- | 'id'
instance From a a where
  from = id

-- | 'const'
instance From a (x -> a) where
  from = const

-- | 'pure'
instance From a [a] where
  from = pure

-- | 'Just'
instance From a (Maybe a) where
  from = Just

-- | 'Left'
instance From a (Either a x) where
  from = Left

-- | 'Right'
instance From a (Either x a) where
  from = Right

-- | 'Void.absurd'
instance From Void.Void x where
  from = Void.absurd

-- | 'fst'
instance From (a, x) a where
  from = fst

-- | 'snd'
instance From (x, a) a where
  from = snd

-- | 'Tuple.swap'
instance From (a, b) (b, a) where
  from = Tuple.swap

-- | 'NonEmpty.toList'
instance From (NonEmpty.NonEmpty a) [a] where
  from = NonEmpty.toList

-- | 'fromIntegral'
instance From Word.Word8 Word.Word16 where
  from = fromIntegral

-- | 'fromIntegral'
instance From Word.Word16 Word.Word32 where
  from = fromIntegral

-- | 'fromIntegral'
instance From Word.Word32 Word.Word64 where
  from = fromIntegral

-- | 'fromIntegral'
instance From Word Natural.Natural where
  from = fromIntegral

-- | 'fromIntegral'
instance From Natural.Natural Integer where
  from = fromIntegral

-- | 'fromIntegral'
instance From Int.Int8 Int.Int16 where
  from = fromIntegral

-- | 'fromIntegral'
instance From Int.Int16 Int.Int32 where
  from = fromIntegral

-- | 'fromIntegral'
instance From Int.Int32 Int.Int64 where
  from = fromIntegral

-- | 'fromIntegral'
instance From Int Integer where
  from = fromIntegral

-- | 'fromIntegral'
instance From Integer Rational where
  from = fromIntegral

-- | 'realToFrac'
instance From Float Double where
  from = realToFrac

-- | 'fromEnum'
instance From Bool Int where
  from = fromEnum

-- | 'fromEnum'
instance From Char Int where
  from = fromEnum

-- | 'ByteString.pack'
instance From [Word.Word8] ByteString.ByteString where
  from = ByteString.pack

-- | 'ByteString.unpack'
instance From ByteString.ByteString [Word.Word8] where
  from = ByteString.unpack

-- | 'LazyByteString.fromStrict'
instance From ByteString.ByteString LazyByteString.ByteString where
  from = LazyByteString.fromStrict

-- | 'LazyByteString.toStrict'
instance From LazyByteString.ByteString ByteString.ByteString where
  from = LazyByteString.toStrict

-- | 'Text.pack'
instance From String Text.Text where
  from = Text.pack

-- | 'Text.unpack'
instance From Text.Text String where
  from = Text.unpack

-- | 'LazyText.fromStrict'
instance From Text.Text LazyText.Text where
  from = LazyText.fromStrict

-- | 'LazyText.toStrict'
instance From LazyText.Text Text.Text where
  from = LazyText.toStrict

-- | 'Seq.fromList'
instance From [a] (Seq.Seq a) where
  from = Seq.fromList

-- | 'Foldable.toList'
instance From (Seq.Seq a) [a] where
  from = Foldable.toList

-- | 'Set.fromList'
--
-- Note that this will remove duplicate elements from the list.
instance Ord a => From [a] (Set.Set a) where
  from = Set.fromList

-- | 'Set.toAscList'
instance From (Set.Set a) [a] where
  from = Set.toAscList

-- | 'Map.fromList'
--
-- Note that if there are duplicate keys in the list, the one closest to the
-- end will win.
instance Ord k => From [(k, v)] (Map.Map k v) where
  from = Map.fromList

-- | 'Map.toAscList'
instance From (Map.Map k v) [(k, v)] where
  from = Map.toAscList
