{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

-- | This module provides the 'Cast' type class for converting values between
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
module Witch (Cast(cast), from, into, via) where

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
-- another type. The constraint @Cast a b@ means that you can convert from a
-- value of type @a@ into a value of type @b@.
--
-- This is primarily intended for "zero cost" conversions like @newtype@s. For
-- example if you wanted to have a type to represent someone's name, you could
-- say:
--
-- > newtype Name = Name String
-- > instance Cast String Name
-- > instance Cast Name String
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
--   Avoid writing instances like @Cast Int (Maybe Char)@. (It might be
--   worthwhile to have a separate @TryCast@ type class for this.)
--
-- - Conversions should be unambiguous. For example there are many ways to
--   decode a @ByteString@ into @Text@, so you shouldn't provide an instance
--   for that.
--
-- - Conversions should be cheap, ideally free. For example converting from
--   @String@ to @Text@ is probably fine, but converting from a UTF-8 encoded
--   @ByteString@ to @Text@ is problematic.
--
-- - Conversions should be lossless. In other words if you have @Cast a b@ then
--   no two @a@ values should be converted to the same @b@ value. For example
--   @Cast Int Integer@ is fine because every @Int@ can be mapped to a
--   corresponding @Integer@, but @Cast Integer Int@ is not good because some
--   @Integer@s are out of bounds and would need to be clamped.
--
-- - If you have both @Cast a b@ and @Cast b a@, then @cast . cast@ should be
--   the same as 'id'. In other words @a@ and @b@ are isomorphic.
--
-- - If you have both @Cast a b@ and @Cast b c@, then it's up to you if you
--   want to provide @Cast a c@. Sometimes using 'via' is ergonomic enough,
--   other times you want the extra instance. (It would be nice if we could
--   provide @instance (Cast a b, Cast b c) => Cast a c where cast = via \@b@.)
class Cast source target where
  -- | This method implements the conversion of a value between types. In
  -- practice most instances don't need an explicit implementation. At call
  -- sites you'll usually want to use 'from' or 'into' instead of 'cast'.
  --
  -- The default implementation of 'cast' simply calls 'Coerce.coerce', which
  -- works for types that have the same runtime representation.
  cast :: source -> target
  default cast :: Coerce.Coercible source target => source -> target
  cast = Coerce.coerce

-- This ugly hack is used to require type applications when calling 'from' and
-- 'into'. See <https://twitter.com/taylorfausak/status/1329084033003782148>.
data Never
type family Ambiguous a where
  Ambiguous Never = ()
  Ambiguous a = a

-- | This function converts a value from one type into another. This is
-- intended to be used with the @TypeApplications@ language extension. The
-- @Ambiguous@ type in the signature makes a type application required. If
-- you'd prefer not to provide a type application, use 'cast' instead.
--
-- As an example, here are a few ways to convert from an 'Int' into an
-- 'Integer':
--
-- > from @Int @Integer 123
-- > from @_ @Integer (123 :: Int)
-- > from @Int @_ 123 :: Integer
-- > from @Int 123 :: Integer
--
-- Often the context around an expression will make the explicit type
-- signatures unnecessary. If you find yourself using a partial type
-- signature, consider using 'into' instead. For example:
--
-- > let someInt = 123 :: Int
-- > from @_ @Integer someInt -- avoid this
-- > into @Integer someInt -- prefer this
--
from :: forall s target source . (Ambiguous s ~ source, Cast source target) => source -> target
from = cast

-- | This function converts a value from one type into another. This is the
-- same as 'from' except that the type variables are in the opposite order.
into :: forall t source target . (Ambiguous t ~ target, Cast source target) => source -> target
into = cast

-- | This function converts a value from one type into another by going through
-- some third type. This is the same as calling 'cast' (or 'from' or 'into')
-- twice, but can sometimes be more convenient.
--
-- Note that the type in the middle of the conversion is the first type
-- variable of this function. In other words, @via \@b \@a \@c@ first converts
-- from @a@ to @b@, and then from @b@ to @c@. Often both @a@ and @c@ will be
-- inferred from context, which means you can just write @via \@b@.
via :: forall through source target . (Cast source through, Cast through target) => source -> target
via = cast . (\ x -> x :: through) . cast

-- | 'id'
instance Cast a a where
  cast = id

-- | 'const'
instance Cast a (x -> a) where
  cast = const

-- | 'pure'
instance Cast a [a] where
  cast = pure

-- | 'Just'
instance Cast a (Maybe a) where
  cast = Just

-- | 'Left'
instance Cast a (Either a x) where
  cast = Left

-- | 'Right'
instance Cast a (Either x a) where
  cast = Right

-- | 'Void.absurd'
instance Cast Void.Void x where
  cast = Void.absurd

-- | 'fst'
instance Cast (a, x) a where
  cast = fst

-- | 'snd'
instance Cast (x, a) a where
  cast = snd

-- | 'Tuple.swap'
instance Cast (a, b) (b, a) where
  cast = Tuple.swap

-- | 'NonEmpty.toList'
instance Cast (NonEmpty.NonEmpty a) [a] where
  cast = NonEmpty.toList

-- | 'fromIntegral'
instance Cast Word.Word8 Word.Word16 where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Word.Word16 Word.Word32 where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Word.Word32 Word.Word64 where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Word Natural.Natural where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Natural.Natural Integer where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Int.Int8 Int.Int16 where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Int.Int16 Int.Int32 where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Int.Int32 Int.Int64 where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Int Integer where
  cast = fromIntegral

-- | 'fromIntegral'
instance Cast Integer Rational where
  cast = fromIntegral

-- | 'realToFrac'
instance Cast Float Double where
  cast = realToFrac

-- | 'fromEnum'
instance Cast Bool Int where
  cast = fromEnum

-- | 'fromEnum'
instance Cast Char Int where
  cast = fromEnum

-- | 'ByteString.pack'
instance Cast [Word.Word8] ByteString.ByteString where
  cast = ByteString.pack

-- | 'ByteString.unpack'
instance Cast ByteString.ByteString [Word.Word8] where
  cast = ByteString.unpack

-- | 'LazyByteString.fromStrict'
instance Cast ByteString.ByteString LazyByteString.ByteString where
  cast = LazyByteString.fromStrict

-- | 'LazyByteString.toStrict'
instance Cast LazyByteString.ByteString ByteString.ByteString where
  cast = LazyByteString.toStrict

-- | 'Text.pack'
--
-- Note that some 'Char' values cannot be represented in 'Text' and will be
-- replaced by U+FFFD.
instance Cast String Text.Text where
  cast = Text.pack

-- | 'Text.unpack'
instance Cast Text.Text String where
  cast = Text.unpack

-- | 'LazyText.fromStrict'
instance Cast Text.Text LazyText.Text where
  cast = LazyText.fromStrict

-- | 'LazyText.toStrict'
instance Cast LazyText.Text Text.Text where
  cast = LazyText.toStrict

-- | 'Seq.fromList'
instance Cast [a] (Seq.Seq a) where
  cast = Seq.fromList

-- | 'Foldable.toList'
instance Cast (Seq.Seq a) [a] where
  cast = Foldable.toList

-- | 'Set.fromList'
--
-- Note that this will remove duplicate elements from the list.
instance Ord a => Cast [a] (Set.Set a) where
  cast = Set.fromList

-- | 'Set.toAscList'
instance Cast (Set.Set a) [a] where
  cast = Set.toAscList

-- | 'Map.fromList'
--
-- Note that if there are duplicate keys in the list, the one closest to the
-- end will win.
instance Ord k => Cast [(k, v)] (Map.Map k v) where
  cast = Map.fromList

-- | 'Map.toAscList'
instance Cast (Map.Map k v) [(k, v)] where
  cast = Map.toAscList
