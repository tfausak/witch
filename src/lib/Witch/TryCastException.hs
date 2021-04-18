{-# LANGUAGE ScopedTypeVariables #-}

module Witch.TryCastException where

import qualified Control.Exception as Exception
import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable

-- | This exception is thrown when a @TryCast@ conversion fails. It has the
-- original @source@ value that caused the failure and it knows the @target@
-- type it was trying to convert into.
newtype TryCastException source target
  = TryCastException source
  deriving Eq

instance
  ( Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => Show (TryCastException source target) where
  showsPrec d (TryCastException x) =
    showParen (d > 10)
      $ showString "TryCastException {- "
      . shows
          (Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy (source -> target)))
      . showString " -} "
      . showsPrec 11 x

instance
  ( Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => Exception.Exception (TryCastException source target)
