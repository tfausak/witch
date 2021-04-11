{-# LANGUAGE MultiParamTypeClasses #-}

module Witch.TryCast where

import qualified Witch.TryCastException as TryCastException

class TryCast source target where
  tryCast :: source -> Either (TryCastException.TryCastException source target) target
