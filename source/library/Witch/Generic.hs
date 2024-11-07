{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Witch.Generic where

import qualified GHC.Generics as Generics
import qualified Witch.From as From

class GFrom s t where
  gFrom :: s x -> t x

instance GFrom Generics.V1 Generics.V1 where
  gFrom = id

instance GFrom Generics.U1 Generics.U1 where
  gFrom = id

instance (From.From s t) => GFrom (Generics.K1 a s) (Generics.K1 b t) where
  gFrom = Generics.K1 . From.from . Generics.unK1

instance (GFrom s t) => GFrom (Generics.M1 a b s) (Generics.M1 c d t) where
  gFrom = Generics.M1 . gFrom . Generics.unM1

instance (GFrom s1 t1, GFrom s2 t2) => GFrom (s1 Generics.:+: s2) (t1 Generics.:+: t2) where
  gFrom x = case x of
    Generics.L1 l -> Generics.L1 $ gFrom l
    Generics.R1 r -> Generics.R1 $ gFrom r

instance (GFrom s1 t1, GFrom s2 t2) => GFrom (s1 Generics.:*: s2) (t1 Generics.:*: t2) where
  gFrom (l Generics.:*: r) = gFrom l Generics.:*: gFrom r

instance
  ( Generics.Generic s,
    Generics.Generic t,
    GFrom (Generics.Rep s) (Generics.Rep t)
  ) =>
  From.From s (Generics.Generically t)
  where
  from = Generics.Generically . Generics.to . gFrom . Generics.from
