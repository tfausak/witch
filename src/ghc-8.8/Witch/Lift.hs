{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Witch.Lift where

import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch.Identity as Identity
import qualified Witch.TryCast as TryCast

liftedCast
  :: forall source target
  . ( TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => source
  -> TH.Q (TH.TExp target)
liftedCast s = case TryCast.tryCast s of
  Left e -> Exception.throw e
  Right t -> TH.unsafeTExpCoerce $ TH.lift (t :: target)

liftedFrom
  :: forall s target source
  . ( Identity.Identity s ~ source
  , TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => source
  -> TH.Q (TH.TExp target)
liftedFrom = liftedCast

liftedInto
  :: forall t source target
  . ( Identity.Identity t ~ target
  , TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => source
  -> TH.Q (TH.TExp target)
liftedInto = liftedCast
