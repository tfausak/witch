{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.TryCast where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Witch.TryCastException as TryCastException

class TryCast source target where
  tryCast :: source -> Either (TryCastException.TryCastException source target) target

instance TryCast [a] (NonEmpty.NonEmpty a) where
  tryCast = maybeTryCast NonEmpty.nonEmpty

instance TryCast Int.Int16 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int.Int32 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int.Int32 Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int.Int32 Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int.Int64 Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int.Int64 Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int.Int64 Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int.Int64 Int where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Int Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Integer Int.Int8 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Integer Int.Int16 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Integer Int.Int32 where
  tryCast = maybeTryCast Bits.toIntegralSized

instance TryCast Integer Int.Int64 where
  tryCast = maybeTryCast Bits.toIntegralSized

maybeTryCast :: (s -> Maybe t) -> s -> Either (TryCastException.TryCastException s t) t
maybeTryCast f s = case f s of
  Nothing -> Left $ TryCastException.TryCastException s
  Just t -> Right t
