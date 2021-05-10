{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.From where

import qualified Data.Coerce as Coerce

-- | This type class is for converting values from some @source@ type into
-- some other @target@ type. The constraint @From source target@ measn that
-- you can convert from a value of type @source@ into a value of type
-- @target@.
--
-- This type class is for conversions that cannot fail. If your conversion can
-- fail, consider implementing @TryCast@ instead.
class From source target where
  -- | This method implements the conversion of a value between types. At call
  -- sites you will usually want to use @from@ or @into@ instead of this
  -- method.
  --
  -- The default implementation of this method simply calls 'Coerce.coerce',
  -- which works for types that have the same runtime representation. This
  -- means that for @newtype@s you do not need to implement this method at
  -- all. For example:
  --
  -- >>> newtype Name = Name String
  -- >>> instance From Name String
  -- >>> instance From String Name
  from :: source -> target

  default from :: Coerce.Coercible source target => source -> target
  from = Coerce.coerce