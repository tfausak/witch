{-# LANGUAGE DataKinds #-}

module Witch.Encoding where

import qualified Data.Tagged as Tagged

-- | <https://en.wikipedia.org/wiki/ISO/IEC_8859-1>
type ISO_8859_1 = Tagged.Tagged "ISO-8859-1"

-- | <https://en.wikipedia.org/wiki/UTF-8>
type UTF_8 = Tagged.Tagged "UTF-8"
