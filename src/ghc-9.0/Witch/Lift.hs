{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Witch.Lift where

import qualified Control.Exception as Exception
import qualified Control.Monad.IO.Class as IO
import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch.Identity as Identity
import qualified Witch.TryCast as TryCast

liftedCast
  :: ( TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  , TH.Quote m
  ) => source
  -> TH.Code m target
liftedCast = either (TH.liftCode . IO.liftIO . Exception.throwIO) TH.liftTyped . TryCast.tryCast

liftedFrom
  :: forall s m source target
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
  :: forall t m source target
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
