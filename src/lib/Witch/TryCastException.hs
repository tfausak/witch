{-# LANGUAGE ScopedTypeVariables #-}

module Witch.TryCastException where

import qualified Control.Exception as Exception
import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable

newtype TryCastException source target
  = TryCastException source
  deriving (Eq, Show)

instance
  ( Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => Exception.Exception (TryCastException source target) where
  displayException (TryCastException x) = mconcat
    [ "TryCastException: failed to cast value "
    , show x
    , " from type "
    , show $ Typeable.typeOf x
    , " into type "
    , show $ Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy target)
    ]
