{-# LANGUAGE ScopedTypeVariables #-}

module Witch.TryFromException where

import qualified Control.Exception as Exception
import qualified Data.Coerce as Coerce
import qualified Data.Proxy as Proxy
import qualified Data.Typeable as Typeable

-- | This exception is thrown when a @TryFrom@ conversion fails. It has the
-- original @source@ value that caused the failure and it knows the @target@
-- type it was trying to convert into. It also has an optional
-- 'Exception.SomeException' for communicating what went wrong while
-- converting.
data TryFromException source target = TryFromException
  source
  (Maybe Exception.SomeException)

instance
  ( Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => Show (TryFromException source target) where
  showsPrec d (TryFromException x e) =
    showParen (d > 10)
      $ showString "TryFromException @"
      . showsPrec 11 (Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy source))
      . showString " @"
      . showsPrec 11 (Typeable.typeRep (Proxy.Proxy :: Proxy.Proxy target))
      . showChar ' '
      . showsPrec 11 x
      . showChar ' '
      . showsPrec 11 e

instance
  ( Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => Exception.Exception (TryFromException source target)

-- | For internal usage.
-- Replaces the source with the given value.
withSource
  :: source2 -> TryFromException source1 t -> TryFromException source2 t
withSource s (TryFromException _ e) = TryFromException s e

-- | For internal usage.
-- Replaces the target with the given type.
withTarget
  :: forall target2 source target1
   . TryFromException source target1
  -> TryFromException source target2
withTarget = Coerce.coerce
