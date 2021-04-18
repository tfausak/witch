{-# LANGUAGE TypeFamilies #-}

module Witch.Identity where

-- | This is an ugly hack used to make GHC require type applications for
-- certain functions. See this Twitter thread for a discussion:
-- <https://twitter.com/taylorfausak/status/1329084033003782148>.
type family Identity a where
  Identity Never = ()
  Identity a = a

-- | Never use this type for anything! It only exists to make the 'Identity'
-- type family non-trivial.
data Never
