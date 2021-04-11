{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Witch.Lift where

import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch.Identity as Identity
import qualified Witch.TryCast as TryCast
import qualified Witch.Utility as Utility

liftedCast
  :: forall source target m
  . ( TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  , TH.Quote m
  ) => source
  -> TH.Code m target
liftedCast = TH.liftTyped . Utility.unsafeCast

liftedFrom
  :: forall s target m source
  . ( Identity.Identity s ~ source
  , TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  , TH.Quote m
  ) => source
  -> TH.Code m target
liftedFrom = liftedCast

liftedInto
  :: forall t source m target
  . ( Identity.Identity t ~ target
  , TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  , TH.Quote m
  ) => source
  -> TH.Code m target
liftedInto = liftedCast
