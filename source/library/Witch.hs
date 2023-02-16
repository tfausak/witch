-- | The Witch package is a library that allows you to confidently convert
-- values between various types. This module exports everything you need to
-- perform conversions or define your own. It is designed to be imported
-- unqualified, so getting started is as easy as:
--
-- >>> import Witch
--
-- In typical usage, the functions that you will use most often are
-- 'Witch.Utility.into' for conversions that always succeed and
-- 'Witch.Utility.tryInto' for conversions that sometimes fail.
--
-- Please consider reading the blog post that announces this library:
-- <https://taylor.fausak.me/2021/07/13/witch/>
module Witch
  ( -- * Type classes

    -- ** From
    Witch.From.From (from),
    Witch.Utility.into,

    -- ** TryFrom
    Witch.TryFrom.TryFrom (tryFrom),
    Witch.Utility.tryInto,

    -- * Data types
    Witch.TryFromException.TryFromException (..),

    -- ** Encodings
    Witch.Encoding.ISO_8859_1,
    Witch.Encoding.UTF_8,
    Witch.Encoding.UTF_16LE,
    Witch.Encoding.UTF_16BE,
    Witch.Encoding.UTF_32LE,
    Witch.Encoding.UTF_32BE,

    -- * Utilities
    Witch.Utility.over,
    Witch.Utility.via,
    Witch.Utility.tryVia,
    Witch.Utility.maybeTryFrom,
    Witch.Utility.eitherTryFrom,

    -- ** Unsafe

    -- | These functions should only be used in two circumstances: When you know
    -- a conversion is safe even though you can't prove it to the compiler, and
    -- when you're alright with your program crashing if the conversion fails.
    -- In all other cases you should prefer the normal conversion functions like
    -- 'Witch.TryFrom.tryFrom'. And if you're converting a literal value,
    -- consider using the Template Haskell conversion functions like
    -- 'Witch.Lift.liftedFrom'.
    Witch.Utility.unsafeFrom,
    Witch.Utility.unsafeInto,

    -- ** Template Haskell

    -- | This library uses /typed/ Template Haskell, which may be a little
    -- different than what you're used to. Normally Template Haskell uses the
    -- @$(...)@ syntax for splicing in things to run at compile time. The typed
    -- variant uses the @$$(...)@ syntax for splices, doubling up on the dollar
    -- signs. Other than that, using typed Template Haskell should be pretty
    -- much the same as using regular Template Haskell.
    Witch.Lift.liftedFrom,
    Witch.Lift.liftedInto,

    -- * Notes

    -- ** Motivation

    -- | Haskell provides many ways to convert between common types, and core
    -- libraries add even more. It can be challenging to know which function to
    -- use when converting from some source type @a@ to some target type @b@. It
    -- can be even harder to know if that conversion is safe or if there are any
    -- pitfalls to watch out for.
    --
    -- This library tries to address that problem by providing a common
    -- interface for converting between types. The 'Witch.From.From' type class
    -- is for conversions that cannot fail, and the 'Witch.TryFrom.TryFrom' type
    -- class is for conversions that can fail. These type classes are inspired
    -- by the [@From@](https://doc.rust-lang.org/std/convert/trait.From.html)
    -- trait in Rust.

    -- ** Type applications

    -- | Although you can use this library without the [@TypeApplications@](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/type_applications.html)
    -- language extension, the extension is strongly recommended. Since most
    -- functions provided by this library are polymorphic in at least one type
    -- variable, it's easy to use them in a situation that would be ambiguous.
    -- Normally you could resolve the ambiguity with an explicit type signature,
    -- but type applications are much more ergonomic. For example:
    --
    -- > -- Avoid this:
    -- > f . (from :: Int8 -> Int16) . g
    -- >
    -- > -- Prefer this:
    -- > f . from @Int8 @Int16 . g
    --
    -- Most functions in this library have two versions with their type
    -- variables in opposite orders. That's because usually one side of the
    -- conversion or the other already has its type inferred by context. In
    -- those situations it makes sense to only provide one type argument.
    --
    -- > -- Avoid this: (assuming f :: Int16 -> ...)
    -- > f $ from @Int8 @Int16 0
    -- >
    -- > -- Prefer this:
    -- > f $ from @Int8 0
    --
    -- > -- Avoid this: (assuming x :: Int8)
    -- > g $ from @Int8 @Int16 x
    -- >
    -- > -- Prefer this:
    -- > g $ into @Int16 x

    -- ** Alternatives

    -- | Many Haskell libraries already provide similar functionality. How is
    -- this library different?
    --
    -- - [@Coercible@](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Coerce.html#t:Coercible):
    --   This type class is solved by the compiler, but it only works for types
    --   that have the same runtime representation. This is very convenient for
    --   @newtype@s, but it does not work for converting between arbitrary types
    --   like @Int8@ and @Int16@.
    --
    -- - [@Convertible@](https://hackage.haskell.org/package/convertible-1.1.1.0/docs/Data-Convertible-Base.html#t:Convertible):
    --   This popular conversion type class is similar to what this library
    --   provides. The main difference is that it does not differentiate between
    --   conversions that can fail and those that cannot.
    --
    -- - [@From@](https://hackage.haskell.org/package/basement-0.0.11/docs/Basement-From.html#t:From):
    --   This type class is almost identical to what this library provides.
    --   Unfortunately it is part of the @basement@ package, which is an
    --   alternative standard library that some people may not want to depend
    --   on.
    --
    -- - [@Inj@](https://hackage.haskell.org/package/inj-1.0/docs/Inj.html#t:Inj):
    --   This type class requires instances to be an injection, which means that
    --   no two input values should map to the same output. That restriction
    --   prohibits many useful instances. Also many instances throw impure
    --   exceptions.
    --
    -- In addition to those general-purpose type classes, there are many
    -- alternatives for more specific conversions. How does this library compare
    -- to those?
    --
    -- - Monomorphic conversion functions like [@Data.Text.pack@](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:pack)
    --   are explicit but not necessarily convenient. It can be tedious to
    --   manage the imports necessary to use the functions. And if you want to
    --   put them in a custom prelude, you will have to come up with your own
    --   names.
    --
    -- - Polymorphic conversion methods like 'toEnum' are more convenient but
    --   may have unwanted semantics or runtime behavior. For example the 'Enum'
    --   type class is more or less tied to the 'Int' data type and frequently
    --   throws impure exceptions.
    --
    -- - Polymorphic conversion functions like 'fromIntegral' are very
    --   convenient. Unfortunately it can be challenging to know which types
    --   have the instances necessary to make the conversion possible. And even
    --   if the conversion is possible, is it safe? For example converting a
    --   negative 'Int' into a 'Word' will overflow, which may be surprising.

    -- ** Instances

    -- | When should you add a 'Witch.From.From' (or 'Witch.TryFrom.TryFrom')
    -- instance for some pair of types? This is a surprisingly tricky question
    -- to answer precisely. Instances are driven more by guidelines than rules.
    --
    -- - Conversions must not throw impure exceptions. This means no 'undefined'
    --   or anything equivalent to it.
    --
    -- - Conversions should be unambiguous. If there are multiple reasonable
    --   ways to convert from @a@ to @b@, then you probably should not add a
    --   'Witch.From.From' instance for them.
    --
    -- - Conversions should be lossless. If you have @From a b@ then no two @a@
    --   values should be converted to the same @b@ value.
    --
    --     - Some conversions necessarily lose information, like converting from
    --       a list into a set.
    --
    -- - If you have both @From a b@ and @From b a@, then
    --   @from \@b \@a . from \@a \@b@ should be the same as 'id'. In other
    --   words, @a@ and @b@ are isomorphic.
    --
    --     - This often true, but not always. For example, converting a list
    --       into a set will remove duplicates. And then converting back into a
    --       list will put the elements in ascending order.
    --
    -- - If you have both @From a b@ and @From b c@, then you could also have
    --   @From a c@ and it should be the same as @from \@b \@c . from \@a \@b@.
    --   In other words, @From@ is transitive.
    --
    --     - This is not always true. For example an @Int8@ may be represented
    --       as a number in JSON, whereas an @Int64@ might be represented as a
    --       string. That means @into \@JSON (into \@Int64 int8)@ would not be
    --       the same as @into \@JSON int8@.
    --
    -- - You should not have both a @From@ instance and a @TryFrom@ instance for
    --   the same pair of types.
    --
    -- - If you have a @From@ or @TryFrom@ instance for a pair of types, then
    --   you should probably have a @From@ or @TryFrom@ instance for the same
    --   pair of types but in the opposite direction. In other words if you have
    --   @From a b@ then you should have @From b a@ or @TryFrom b a@.
    --
    -- In general if @s@ /is/ a @t@, then you should add a 'Witch.From.From'
    -- instance for it. But if @s@ merely /can be/ a @t@, then you could add a
    -- 'Witch.TryFrom.TryFrom' instance for it. And if it is technically
    -- possible to convert from @s@ to @t@ but there are a lot of caveats, you
    -- probably should not write any instances at all.

    -- ** Laws

    -- | As the previous section notes, there aren't any cut and dried laws for
    -- the @From@ and @TryFrom@ type classes. However it can be useful to
    -- consider the following equations for guiding instances:
    --
    -- > -- same strictness
    -- > seq (from @a @b x) y = seq x y
    -- > seq (tryFrom @a @b x) y = seq x y
    --
    -- > -- round trip
    -- > from @b @a (from @a @b x) = x
    --
    -- > -- transitive
    -- > from @b @c (from @a @b x) = from @a @c x
    -- > tryFrom @b @a (from @a @b x) = Right x
    -- > if isRight (tryFrom @a @b x) then
    -- >   fmap (from @b @a) (tryFrom @a @b x) = Right x
    -- > if isRight (tryFrom @a @b x) then do
    -- >   fmap (tryFrom @b @a) (tryFrom @a @b x) = Right (Right x)

    -- ** Integral types

    -- | There are a lot of types that represent various different ranges of
    -- integers, and Witch may not provide the instances you want. In particular
    -- it does not provide a total way to convert from an @Int32@ into an @Int@.
    -- Why is that?
    --
    -- The Haskell Language Report only demands that @Int@s have at least 30
    -- bits of precision. That means a reasonable Haskell implementation could
    -- have an @Int@ type that's smaller than the @Int32@ type.
    --
    -- However in practice everyone uses the same Haskell implementation: GHC.
    -- And with GHC the @Int@ type always has 32 bits of precision, even on
    -- 32-bit architectures. So for almost everybody, it's probably safe to use
    -- @unsafeFrom \@Int32 \@Int@. Similarly most software these days runs on
    -- machines with 64-bit architectures. That means it's also probably safe
    -- for you to use @unsafeFrom \@Int64 \@Int@.
    --
    -- All of the above also applies for @Word@, @Word32@, and @Word64@.

    -- ** Downsides

    -- | As the author of this library, I obviously think that everyone should
    -- use it because it's the greatest thing since sliced bread. But nothing is
    -- perfect, so what are some downsides to this library?
    --
    -- - More specific type classes are often better. For example, @IsString s@
    --   is more useful that @From String s@. The former says that the type @s@
    --   is the same as a string literal, but the latter just says you can
    --   produce a value of type @s@ when given a string.
    --
    -- - The @From@ type class works great for specific pairs of types, but can
    --   get confusing when it's polymorphic. For example if you have some
    --   function with a @From s t@ constraint, that doesn't really tell you
    --   anything about what it's doing.
  )
where

import qualified Witch.Encoding
import qualified Witch.From
import Witch.Instances ()
import qualified Witch.Lift
import qualified Witch.TryFrom
import qualified Witch.TryFromException
import qualified Witch.Utility
