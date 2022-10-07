{-# LANGUAGE DataKinds #-}

module Witch.Encoding where

import qualified Data.Tagged as Tagged

-- | <https://en.wikipedia.org/wiki/UTF-8>
type UTF_8 = Tagged.Tagged "UTF-8"
