{-# LANGUAGE ScopedTypeVariables #-}

module Witch.TryCastException where

import qualified Control.Exception as Exception
import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable

newtype TryCastException source target
  = TryCastException source
  deriving Eq

instance
  ( Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => Show (TryCastException source target) where
  showsPrec d (TryCastException x) = showParen (d > 10)
    $ showString "TryCastException {- "
    . shows (Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy (source -> target)))
    . showString " -} "
    . showsPrec 11 x

instance
  ( Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => Exception.Exception (TryCastException source target)
