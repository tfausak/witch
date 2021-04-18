{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Witch.Lift where

import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch.Identity as Identity
import qualified Witch.TryCast as TryCast
import qualified Witch.Utility as Utility

-- | This is like 'Utility.unsafeCast' except that it works at compile time
-- rather than runtime.
--
-- > -- Avoid this:
-- > unsafeCast "some literal"
-- >
-- > -- Prefer this:
-- > $$(liftedCast "some literal")
liftedCast
  :: forall source target m
   . ( TryCast.TryCast source target
     , TH.Lift target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     , TH.Quote m
     )
  => source
  -> TH.Code m target
liftedCast = TH.liftTyped . Utility.unsafeCast

-- | This is like 'Utility.unsafeFrom' except that it works at compile time
-- rather than runtime.
--
-- > -- Avoid this:
-- > unsafeFrom @s "some literal"
-- >
-- > -- Prefer this:
-- > $$(liftedFrom @s "some literal")
liftedFrom
  :: forall s target m source
   . ( Identity.Identity s ~ source
     , TryCast.TryCast source target
     , TH.Lift target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     , TH.Quote m
     )
  => source
  -> TH.Code m target
liftedFrom = liftedCast

-- | This is like 'Utility.unsafeInto' except that it works at compile time
-- rather than runtime.
--
-- > -- Avoid this:
-- > unsafeInto @t "some literal"
-- >
-- > -- Prefer this:
-- > $$(liftedInto @t "some literal")
liftedInto
  :: forall t source m target
   . ( Identity.Identity t ~ target
     , TryCast.TryCast source target
     , TH.Lift target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     , TH.Quote m
     )
  => source
  -> TH.Code m target
liftedInto = liftedCast
