{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.Cast where

import qualified Data.Coerce as Coerce
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty

class Cast source target where
  cast :: source -> target

  default cast :: Coerce.Coercible source target => source -> target
  cast = Coerce.coerce

instance Cast (NonEmpty.NonEmpty a) [a] where
  cast = NonEmpty.toList

instance Cast Int.Int8 Int.Int16 where
  cast = fromIntegral

instance Cast Int.Int8 Int.Int32 where
  cast = fromIntegral

instance Cast Int.Int8 Int.Int64 where
  cast = fromIntegral

instance Cast Int.Int8 Int where
  cast = fromIntegral

instance Cast Int.Int8 Integer where
  cast = fromIntegral

instance Cast Int.Int16 Int.Int32 where
  cast = fromIntegral

instance Cast Int.Int16 Int.Int64 where
  cast = fromIntegral

instance Cast Int.Int16 Int where
  cast = fromIntegral

instance Cast Int.Int16 Integer where
  cast = fromIntegral

instance Cast Int.Int32 Int.Int64 where
  cast = fromIntegral

instance Cast Int.Int32 Integer where
  cast = fromIntegral

instance Cast Int.Int64 Integer where
  cast = fromIntegral

instance Cast Int Int.Int64 where
  cast = fromIntegral

instance Cast Int Integer where
  cast = fromIntegral
