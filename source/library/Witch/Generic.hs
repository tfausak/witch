{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Witch.Generic where

import qualified GHC.Generics as Generics
import qualified Witch.From as From

-- | This type class is used to implement generic conversions using the 'Generics.Generically' helper.
-- This is an advanced use case.
-- Most users will not need to know about this type class.
-- And even for those that want to derive 'Generics.Generically', this type class should be an implementation detail.
--
-- This type class can convert between any two types as long as they have 'Generics.Generic' instances and they are structurally similar.
-- For example, if you define your own empty type you could convert it to the typical 'Data.Void.Void' type:
--
-- > data Empty deriving Generic
-- > deriving via Generically Void instance From Empty Void
--
-- Or your own unit type:
--
-- > data Unit = MkUnit deriving Generic
-- > deriving via Generically () instance From Unit ()
--
-- Note that this looks superficially similar to @newtype Unit = MkUnit ()@ together with @instance From Unit ()@, but that goes through 'Data.Coerce.Coercible' and requires the types to be representationally equal.
-- This approach (with 'Generics.Generically') only requires the types to be /structurally/ equal.
-- In this case, @Unit@ is structurally equal to @()@ since they both have a single constructor with no arguments.
--
-- This also works with arbitrary product types, like a custom pair type:
--
-- > data Pair a b = MkPair a b deriving Generic
-- > deriving via Generically (Pair c d)
-- >   instance (From a c, From b d) => From (a, b) (Pair c d)
--
-- Note that this can also convert the type variables as long as they have 'From.From' instances as well.
-- This allows converting from @(Int, Int)@ to @Pair Integer Integer@ in one step, for example.
--
-- And this works with arbitrary sum types as well:
--
-- > data Result a b = Failure a | Success b deriving Generic
-- > deriving via Generically (Result c d)
-- >   instance (From a c, From b d) => From (Either a b) (Result c d)
--
-- Note that these conversions are all /structural/ not semantic.
-- That means if you had defined @Result@ as @Success b | Failure a@, then converting from 'Either' would be "wrong".
-- 'Left' would convert into @Success@ and 'Right' would convert into @Failure@.
class GFrom s t where
  gFrom :: s x -> t x

instance GFrom Generics.V1 Generics.V1 where
  gFrom = id

instance GFrom Generics.U1 Generics.U1 where
  gFrom = id

instance (From.From s t) => GFrom (Generics.K1 a s) (Generics.K1 b t) where
  gFrom = Generics.K1 . From.from . Generics.unK1

instance (GFrom s t) => GFrom (Generics.M1 a b s) (Generics.M1 c d t) where
  gFrom = Generics.M1 . gFrom . Generics.unM1

instance (GFrom s1 t1, GFrom s2 t2) => GFrom (s1 Generics.:+: s2) (t1 Generics.:+: t2) where
  gFrom x = case x of
    Generics.L1 l -> Generics.L1 $ gFrom l
    Generics.R1 r -> Generics.R1 $ gFrom r

instance (GFrom s1 t1, GFrom s2 t2) => GFrom (s1 Generics.:*: s2) (t1 Generics.:*: t2) where
  gFrom (l Generics.:*: r) = gFrom l Generics.:*: gFrom r

-- | See the 'GFrom' type class for an explanation of this instance.
instance
  ( Generics.Generic s,
    Generics.Generic t,
    GFrom (Generics.Rep s) (Generics.Rep t)
  ) =>
  From.From s (Generics.Generically t)
  where
  from = Generics.Generically . Generics.to . gFrom . Generics.from
