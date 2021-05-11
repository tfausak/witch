{-# LANGUAGE ScopedTypeVariables #-}

module Witch.Lift where

import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch.TryFrom as TryFrom
import qualified Witch.Utility as Utility

-- | This is like 'Utility.unsafeFrom' except that it works at compile time
-- rather than runtime.
--
-- > -- Avoid this:
-- > unsafeFrom "some literal"
-- >
-- > -- Prefer this:
-- > $$(liftedFrom "some literal")
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
liftedFrom s = TH.unsafeTExpCoerce $ TH.lift (Utility.unsafeFrom s :: target)

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
liftedInto = liftedFrom
