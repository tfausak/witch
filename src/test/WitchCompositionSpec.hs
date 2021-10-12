{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeApplications #-}

module WitchCompositionSpec
  ( tests
  ) where

import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import Test.HUnit (Test, (~:), (~?=))
import qualified Witch

tests :: [Test]
tests =
  [ "Composition"
      ~: [ "composeTry"
           ~: let
                f = Witch.tryFrom @Int.Int8 @Natural.Natural
                g = Witch.tryFrom @Int.Int16 @Int.Int8
                h = Witch.tryFrom @Int.Int32 @Int.Int16
                fgh = f `Witch.composeTry` g `Witch.composeTry` h
              in
                [ hush (fgh -1) ~?= Nothing
                , hush (fgh 0) ~?= Just 0
                , hush (fgh 1) ~?= Just 1
                , hush (fgh 127) ~?= Just 127
                , hush (fgh 128) ~?= Nothing
                , hush (fgh 32768) ~?= Nothing
                ]
         , "composeTryRhs and composeTryLhs"
           ~: let
                f = Witch.from @Text.Text @String
                g = Witch.tryFrom @ByteString.ByteString @Text.Text
                h = Witch.from @String @ByteString.ByteString
                fgh = f `Witch.composeTryRhs` g `Witch.composeTryLhs` h
                msg = "Hello, World!"
              in [hush (fgh msg) ~?= Just msg]
         ]
  ]


hush :: Either x a -> Maybe a
hush = either (const Nothing) Just
