{-# LANGUAGE ScopedTypeVariables #-}

module Witch.Lift where

import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch.TryFrom as TryFrom
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
  :: forall source target
   . ( TryFrom.TryFrom source target
     , TH.Lift target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> TH.Q (TH.TExp target)
liftedCast s = TH.unsafeTExpCoerce $ TH.lift (Utility.unsafeCast s :: target)

-- | This is like 'Utility.unsafeFrom' except that it works at compile time
-- rather than runtime.
--
-- > -- Avoid this:
-- > unsafeFrom @s "some literal"
-- >
-- > -- Prefer this:
-- > $$(liftedFrom @s "some literal")
liftedFrom
  :: forall source target
   . ( TryFrom.TryFrom source target
     , TH.Lift target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> TH.Q (TH.TExp target)
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
  :: forall target source
   . ( TryFrom.TryFrom source target
     , TH.Lift target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> TH.Q (TH.TExp target)
liftedInto = liftedCast
