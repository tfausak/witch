{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Witch.Casting where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Function as Function
import qualified Witch.Cast as Cast

-- | This type mostly exists to make it easier to derive instances through
-- some other type. It does this by leaning on the 'Cast.Cast' type class. For
-- example, consider this data type:
--
-- > data Toggle = Off | On
-- > instance Cast Toggle String where
-- >   cast toggle = case toggle of
-- >     Off -> "off"
-- >     On -> "on"
--
-- If you wanted to implement a 'Show' instance for @Toggle@ by going through
-- a 'String', you would need to write something like this:
--
-- > instance Show Toggle where
-- >   show = show . into @String
--
-- That may not very complicated, but at the same time it is not very
-- interesting. And it can be tricky if you want to keep other instances (like
-- 'Read') in sync. That's where the 'Casting' type comes in! You can
-- derive the above instance like so:
--
-- > data Toggle = Off | On
-- >   deriving Show via Casting Toggle String
newtype Casting source target
  = Casting source

-- | Uses @coerce@.
instance Cast.Cast s (Casting s t)

-- | Uses @coerce@.
instance Cast.Cast (Casting s t) s

instance (Cast.Cast t s, Bounded t) => Bounded (Casting s t) where
  maxBound = Cast.cast $ Cast.cast @t @s maxBound
  minBound = Cast.cast $ Cast.cast @t @s minBound

instance (Cast.Cast s t, Cast.Cast t s, Enum t) => Enum (Casting s t) where
  fromEnum = fromEnum . Cast.cast @s @t . Cast.cast
  toEnum = Cast.cast . Cast.cast @t @s . toEnum

instance (Cast.Cast s t, Eq t) => Eq (Casting s t) where
  (==) = Function.on (==) $ Cast.cast @s @t . Cast.cast

instance (Cast.Cast s t, Ord t) => Ord (Casting s t) where
  compare = Function.on compare $ Cast.cast @s @t . Cast.cast

instance (Cast.Cast t s, Read t) => Read (Casting s t) where
  readsPrec = fmap (fmap . Bifunctor.first $ Cast.cast . Cast.cast @t @s) . readsPrec

instance (Cast.Cast s t, Show t) => Show (Casting s t) where
  show = show . Cast.cast @s @t . Cast.cast
