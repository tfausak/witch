{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Witch.Lift where

import qualified Control.Exception as Exception
import qualified Control.Monad.IO.Class as IO
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
  -> TH.Q TH.Exp
liftedCast s = case TryCast.tryCast s of
  Left e -> IO.liftIO $ Exception.throwIO e
  Right t -> TH.lift (t :: target)

liftedFrom
  :: forall s source target
  . ( Identity.Identity s ~ source
  , TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => source
  -> TH.Q TH.Exp
liftedFrom = liftedCast @source @target

liftedInto
  :: forall t source target
  . ( Identity.Identity t ~ target
  , TryCast.TryCast source target
  , TH.Lift target
  , Show source
  , Typeable.Typeable source
  , Typeable.Typeable target
  ) => source
  -> TH.Q TH.Exp
liftedInto = liftedCast @source @target
