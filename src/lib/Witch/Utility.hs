{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Witch.Utility where

import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable
import qualified GHC.Stack as Stack
import qualified Witch.Cast as Cast
import qualified Witch.Identity as Identity
import qualified Witch.TryCast as TryCast
import qualified Witch.TryCastException as TryCastException

as :: forall s source . Identity.Identity s ~ source => source -> source
as = id

from
  :: forall s target source
   . (Identity.Identity s ~ source, Cast.Cast source target)
  => source
  -> target
from = Cast.cast

into
  :: forall t source target
   . (Identity.Identity t ~ target, Cast.Cast source target)
  => source
  -> target
into = Cast.cast

over
  :: forall t source target
   . ( Identity.Identity t ~ target
     , Cast.Cast source target
     , Cast.Cast target source
     )
  => (target -> target)
  -> source
  -> source
over f = Cast.cast . f . Cast.cast

via
  :: forall u source target through
   . ( Identity.Identity u ~ through
     , Cast.Cast source through
     , Cast.Cast through target
     )
  => source
  -> target
via = Cast.cast . (\x -> x :: through) . Cast.cast

tryFrom
  :: forall s target source
   . (Identity.Identity s ~ source, TryCast.TryCast source target)
  => source
  -> Either (TryCastException.TryCastException source target) target
tryFrom = TryCast.tryCast

tryInto
  :: forall t source target
   . (Identity.Identity t ~ target, TryCast.TryCast source target)
  => source
  -> Either (TryCastException.TryCastException source target) target
tryInto = TryCast.tryCast

unsafeCast
  :: forall source target
   . ( Stack.HasCallStack
     , TryCast.TryCast source target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> target
unsafeCast = either Exception.throw id . TryCast.tryCast

unsafeFrom
  :: forall s target source
   . ( Identity.Identity s ~ source
     , Stack.HasCallStack
     , TryCast.TryCast source target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> target
unsafeFrom = unsafeCast

unsafeInto
  :: forall t source target
   . ( Identity.Identity t ~ target
     , Stack.HasCallStack
     , TryCast.TryCast source target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> target
unsafeInto = unsafeCast
