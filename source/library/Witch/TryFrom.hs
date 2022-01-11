{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.TryFrom where

import qualified Witch.TryFromException as TryFromException

-- | This type class is for converting values from some @source@ type into
-- some other @target@ type. The constraint @'TryFrom' source target@ means
-- that you may be able to convert from a value of type @source@ into a value
-- of type @target@, but that conversion may fail at runtime.
--
-- This type class is for conversions that can sometimes fail. If your
-- conversion always succeeds, consider implementing @From@ instead.
class TryFrom source target where
  -- | This method implements the conversion of a value between types. At call
  -- sites you may want to use @tryInto@ instead.
  --
  -- > -- Avoid this:
  -- > tryFrom (x :: s)
  -- >
  -- > -- Prefer this:
  -- > tryFrom @s
  --
  -- Consider using @maybeTryFrom@ or @eitherTryFrom@ to implement this
  -- method.
  tryFrom :: source -> Either (TryFromException.TryFromException source target) target
