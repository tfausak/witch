{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.TryCast where

import qualified Witch.TryCastException as TryCastException

-- | This type class is for converting values from some @source@ type into
-- some other @target@ type. The constraint @TryCast source target@ means that
-- you may be able to convert from a value of type @source@ into a value of
-- type @target@, but that conversion may fail at runtime.
--
-- This type class is for conversions that can fail. If your conversion cannot
-- fail, consider implementing @Cast@ instead.
class TryCast source target where
  -- | This method implements the conversion of a value between types. At call
  -- sites you will usually want to use @tryFrom@ or @tryInto@ instead of this
  -- method.
  tryCast :: source -> Either (TryCastException.TryCastException source target) target
