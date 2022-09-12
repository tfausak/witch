{-# LANGUAGE DataKinds #-}

module Witch.Encoding where

import qualified Data.Tagged as Tagged

type Utf8 = Tagged.Tagged "UTF-8"
