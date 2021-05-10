-- | The Witch package is a library that allows you to confidently convert
-- values between various types. This module exports everything you need to
-- perform conversions or define your own. It is designed to be imported
-- unqualified, so getting started is as easy as:
--
-- >>> import Witch
--
-- In typical usage, you will most likely use 'Witch.Utility.into' for
-- 'Witch.From.From' instances and 'With.Utility.tryInto' for
-- 'Witch.TryFrom.TryFrom' instances.
module Witch
  ( -- * Type classes

  -- ** From
    Witch.From.From(from)
  , Witch.Utility.into

  -- ** TryFrom
  , Witch.TryFrom.TryFrom(tryFrom)
  , Witch.Utility.tryInto

  -- * Utilities
  , Witch.Utility.as
  , Witch.Utility.over
  , Witch.Utility.via
  , Witch.Utility.tryVia
  , Witch.Utility.maybeTryCast
  , Witch.Utility.eitherTryCast

  -- ** Unsafe
  -- | These functions should only be used in two circumstances: When you know
  -- a conversion is safe even though you can't prove it to the compiler, and
  -- when you're alright with your program crashing if the conversion fails.
  -- In all other cases you should prefer the normal conversion functions like
  -- 'Witch.From.from'. And if you're converting a literal value, consider
  -- using the Template Haskell conversion functions like
  -- 'Witch.Lift.liftedCast'.
  , Witch.Utility.unsafeCast
  , Witch.Utility.unsafeFrom
  , Witch.Utility.unsafeInto

  -- ** Template Haskell
  -- | This library uses /typed/ Template Haskell, which may be a little
  -- different than what you're used to. Normally Template Haskell uses the
  -- @$(...)@ syntax for splicing in things to run at compile time. The typed
  -- variant uses the @$$(...)@ syntax for splices, doubling up on the dollar
  -- signs. Other than that, using typed Template Haskell should be pretty
  -- much the same as using regular Template Haskell.
  , Witch.Lift.liftedCast
  , Witch.Lift.liftedFrom
  , Witch.Lift.liftedInto

  -- * Data types
  , Witch.TryCastException.TryCastException(..)

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
  --   - Some conversions necessarily lose information, like converting from a
  --     list into a set.
  --
  -- - If you have both @From a b@ and @From b a@, then
  --   @from \@b \@a . from \@a \@b@ should be the same as 'id'. In other
  --   words, @a@ and @b@ are isomorphic.
  --
  --   - This often true, but not always. For example, converting a list into
  --     a set will remove duplicates. And then converting back into a list
  --     will put the elements in ascending order.
  --
  -- - If you have both @From a b@ and @From b c@, then you could also have
  --   @From a c@ and it should be the same as @from \@b \@c . from \@a \@b@.
  --   In other words, @From@ is transitive.
  --
  --   - This is not always true. For example an @Int8@ may be represented as
  --     a number in JSON, whereas an @Int64@ might be represented as a
  --     string. That means @into \@JSON (into \@Int64 int8)@ would not be the
  --     same as @into \@JSON int8@.
  --
  -- In general if @s@ /is/ a @t@, then you should add a 'Witch.From.From'
  -- instance for it. But if @s@ merely /can be/ a @t@, then you could add a
  -- 'Witch.TryFrom.TryFrom' instance for it. And if it is technically
  -- possible to convert from @s@ to @t@ but there are a lot of caveats, you
  -- probably should not write any instances at all.
  ) where

import qualified Witch.From
import Witch.Instances ()
import qualified Witch.Lift
import qualified Witch.TryFrom
import qualified Witch.TryCastException
import qualified Witch.Utility
