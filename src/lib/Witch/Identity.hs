{-# LANGUAGE TypeFamilies #-}

module Witch.Identity where

type family Identity a where
  Identity Never = ()
  Identity a = a

data Never
