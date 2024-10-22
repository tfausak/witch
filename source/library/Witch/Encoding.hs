{-# LANGUAGE DataKinds #-}

module Witch.Encoding where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Tagged as Tagged

-- | <https://en.wikipedia.org/wiki/ISO/IEC_8859-1>
type ISO_8859_1 = Tagged.Tagged "ISO-8859-1"

-- | The 'ISO_8859_1' encoding for strict 'ByteString.ByteString's.
type Latin1S = ISO_8859_1 ByteString.ByteString

-- | The 'ISO_8859_1' encoding for lazy 'LazyByteString.ByteString's.
type Latin1L = ISO_8859_1 LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-8>
type UTF_8 = Tagged.Tagged "UTF-8"

-- | The 'UTF_8' encoding for strict 'ByteString.ByteString's.
type Utf8S = UTF_8 ByteString.ByteString

-- | The 'UTF_8' encoding for lazy 'LazyByteString.ByteString's.
type Utf8L = UTF_8 LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-16>
type UTF_16LE = Tagged.Tagged "UTF-16LE"

-- | The 'UTF_16LE' encoding for strict 'ByteString.ByteString's.
type Utf16LS = UTF_16LE ByteString.ByteString

-- | The 'UTF_16LE' encoding for lazy 'LazyByteString.ByteString's.
type Utf16LL = UTF_16LE LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-16>
type UTF_16BE = Tagged.Tagged "UTF-16BE"

-- | The 'UTF_16BE' encoding for strict 'ByteString.ByteString's.
type Utf16BS = UTF_16BE ByteString.ByteString

-- | The 'UTF_16BE' encoding for lazy 'LazyByteString.ByteString's.
type Utf16BL = UTF_16BE LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-32>
type UTF_32LE = Tagged.Tagged "UTF-32LE"

-- | The 'UTF_32LE' encoding for strict 'ByteString.ByteString's.
type Utf32LS = UTF_32LE ByteString.ByteString

-- | The 'UTF_32LE' encoding for lazy 'LazyByteString.ByteString's.
type Utf32LL = UTF_32LE LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-32>
type UTF_32BE = Tagged.Tagged "UTF-32BE"

-- | The 'UTF_32BE' encoding for strict 'ByteString.ByteString's.
type Utf32BS = UTF_32BE ByteString.ByteString

-- | The 'UTF_32BE' encoding for lazy 'LazyByteString.ByteString's.
type Utf32BL = UTF_32BE LazyByteString.ByteString
