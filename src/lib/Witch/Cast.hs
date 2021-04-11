{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.Cast where

import qualified Data.Coerce as Coerce

class Cast source target where
  cast :: source -> target

  default cast :: Coerce.Coercible source target => source -> target
  cast = Coerce.coerce
