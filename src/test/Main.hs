{-# language TypeApplications #-}

module Main (main) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Typeable as Typeable
import From (from, into, via)
import qualified GHC.Stack as Stack

main :: Stack.HasCallStack => IO ()
main = do
  from 'a' ==> 'a'
  into 'a' ==> 'a'
  via @Char 'a' ==> 'a'

  into @Int False ==> 0
  into @Int True ==> 1

(==>) :: (Stack.HasCallStack, Eq a, Show a, Typeable.Typeable a) => a -> a -> IO ()
actual ==> expected = Monad.when (actual /= expected) . Exception.throwIO $
  Failure expected actual Stack.callStack

data Failure a = Failure a a Stack.CallStack

instance Show a => Show (Failure a) where
  show (Failure expected actual callStack) =
    "Failure!\n"
    <> "  expected: " <> show expected <> "\n"
    <> "  but got:  " <> show actual <> "\n"
    <> Stack.prettyCallStack callStack

instance (Show a, Typeable.Typeable a) => Exception.Exception (Failure a)
