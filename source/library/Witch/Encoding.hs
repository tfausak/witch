{-# LANGUAGE DataKinds #-}

module Witch.Encoding where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Tagged as Tagged

-- | <https://en.wikipedia.org/wiki/ISO/IEC_8859-1>
type ISO_8859_1 = Tagged.Tagged "ISO-8859-1"

type LATIN_1 = ISO_8859_1

type Latin1Strict = LATIN_1 ByteString.ByteString

type Latin1Lazy = LATIN_1 LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-8>
type UTF_8 = Tagged.Tagged "UTF-8"

type Utf8Strict = UTF_8 ByteString.ByteString

type Utf8Lazy = UTF_8 LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-16>
type UTF_16LE = Tagged.Tagged "UTF-16LE"

type Utf16leStrict = UTF_16LE ByteString.ByteString

type Utf16leLazy = UTF_16LE LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-16>
type UTF_16BE = Tagged.Tagged "UTF-16BE"

type Utf16beStrict = UTF_16BE ByteString.ByteString

type Utf16beLazy = UTF_16BE LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-32>
type UTF_32LE = Tagged.Tagged "UTF-32LE"

type Utf32leStrict = UTF_32LE ByteString.ByteString

type Utf32leLazy = UTF_32LE LazyByteString.ByteString

-- | <https://en.wikipedia.org/wiki/UTF-32>
type UTF_32BE = Tagged.Tagged "UTF-32BE"

type Utf32beStrict = UTF_32BE ByteString.ByteString

type Utf32beLazy = UTF_32BE LazyByteString.ByteString
