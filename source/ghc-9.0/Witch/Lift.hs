{-# LANGUAGE ExplicitForAll #-}

module Witch.Lift where

import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch.TryFrom as TryFrom
import qualified Witch.Utility as Utility

-- | This is like 'Utility.unsafeFrom' except that it works at compile time
-- rather than runtime.
--
-- > -- Avoid this:
-- > unsafeFrom @s "some literal"
-- >
-- > -- Prefer this:
-- > $$(liftedFrom @s "some literal")
liftedFrom ::
  forall source target m.
  ( TryFrom.TryFrom source target,
    TH.Lift target,
    Show source,
    Typeable.Typeable source,
    Typeable.Typeable target,
    TH.Quote m
  ) =>
  source ->
  TH.Code m target
liftedFrom = TH.liftTyped . Utility.unsafeFrom

-- | This is like 'Utility.unsafeInto' except that it works at compile time
-- rather than runtime.
--
-- > -- Avoid this:
-- > unsafeInto @t "some literal"
-- >
-- > -- Prefer this:
-- > $$(liftedInto @t "some literal")
liftedInto ::
  forall target source m.
  ( TryFrom.TryFrom source target,
    TH.Lift target,
    Show source,
    Typeable.Typeable source,
    Typeable.Typeable target,
    TH.Quote m
  ) =>
  source ->
  TH.Code m target
liftedInto = liftedFrom
