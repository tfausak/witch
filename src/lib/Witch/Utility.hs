{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Witch.Utility where

import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable
import qualified GHC.Stack as Stack
import qualified Witch.From as From
import qualified Witch.TryCast as TryCast
import qualified Witch.TryCastException as TryCastException

-- | This is the same as 'id' except that it requires a type application. This
-- can be an ergonomic way to pin down a polymorphic type in a function
-- pipeline. For example:
--
-- > -- Avoid this:
-- > f . (\ x -> x :: Int) . g
-- >
-- > -- Prefer this:
-- > f . as @Int . g
as :: forall source . source -> source
as = id

-- | This is the same as 'From.from' except that it requires a type
-- application for the @target@ type.
--
-- > -- Avoid this:
-- > from x :: t
-- >
-- > -- Prefer this:
-- > into @t x
into
  :: forall target source
   . From.From source target
  => source
  -> target
into = From.from

-- | This function converts from some @source@ type into some @target@ type,
-- applies the given function, then converts back into the @source@ type. This
-- is useful when you have two types that are isomorphic but some function
-- that only works with one of them.
--
-- > -- Avoid this:
-- > from @t . f . into @t
-- >
-- > -- Prefer this:
-- > over @t f
over
  :: forall target source
   . ( From.From source target
     , From.From target source
     )
  => (target -> target)
  -> source
  -> source
over f = From.from . f . From.from

-- | This function first converts from some @source@ type into some @through@
-- type, and then converts that into some @target@ type. Usually this is used
-- when writing 'From.From' instances. Sometimes this can be used to work
-- around the lack of an instance that should probably exist.
--
-- > -- Avoid this:
-- > from @u . into @u
-- >
-- > -- Prefer this:
-- > via @u
via
  :: forall through source target
   . ( From.From source through
     , From.From through target
     )
  => source
  -> target
via = From.from . (\x -> x :: through) . From.from

-- | This is the same as 'TryCast.tryCast' except that it requires a type
-- application for the @source@ type.
--
-- > -- Avoid this:
-- > tryCast (x :: s)
-- >
-- > -- Prefer this:
-- > tryFrom @s x
tryFrom
  :: forall source target
   . TryCast.TryCast source target
  => source
  -> Either (TryCastException.TryCastException source target) target
tryFrom = TryCast.tryCast

-- | This is the same as 'TryCast.tryCast' except that it requires a type
-- application for the @target@ type.
--
-- > -- Avoid this:
-- > tryCast x :: Either (TryCastException s t) t
-- >
-- > -- Prefer this:
-- > tryInto @t x
tryInto
  :: forall target source
   . TryCast.TryCast source target
  => source
  -> Either (TryCastException.TryCastException source target) target
tryInto = TryCast.tryCast

-- | This is similar to 'via' except that it works with 'TryCast.TryCast'
-- instances instead. This function is especially convenient because juggling
-- the types in the 'TryCastException.TryCastException' can be tedious.
--
-- > -- Avoid this:
-- > case tryInto @u x of
-- >   Left _ -> Left ...
-- >   Right y -> case tryFrom @u y of
-- >     Left _ -> Left ...
-- >     Right z -> ...
-- >
-- > -- Prefer this:
-- > tryVia @u
tryVia
  :: forall through source target
   . ( TryCast.TryCast source through
     , TryCast.TryCast through target
     )
  => source
  -> Either (TryCastException.TryCastException source target) target
tryVia s = case TryCast.tryCast s of
  Left (TryCastException.TryCastException _ e) ->
    Left $ TryCastException.TryCastException s e
  Right u -> case TryCast.tryCast (u :: through) of
    Left (TryCastException.TryCastException _ e) ->
      Left $ TryCastException.TryCastException s e
    Right t -> Right t

-- | This function can be used to implement 'TryCast.tryCast' with a function
-- that returns 'Maybe'. For example:
--
-- > -- Avoid this:
-- > tryCast s = case f s of
-- >   Nothing -> Left $ TryCastException s Nothing
-- >   Just t -> Right t
-- >
-- > -- Prefer this:
-- > tryCast = maybeTryCast f
maybeTryCast
  :: (source -> Maybe target)
  -> source
  -> Either (TryCastException.TryCastException source target) target
maybeTryCast f s = case f s of
  Nothing -> Left $ TryCastException.TryCastException s Nothing
  Just t -> Right t

-- | This function can be used to implement 'TryCast.tryCast' with a function
-- that returns 'Either'. For example:
--
-- > -- Avoid this:
-- > tryCast s = case f s of
-- >   Left e -> Left . TryCastException s . Just $ toException e
-- >   Right t -> Right t
-- >
-- > -- Prefer this:
-- > tryCast = eitherTryCast f
eitherTryCast
  :: Exception.Exception exception
  => (source -> Either exception target)
  -> source
  -> Either (TryCastException.TryCastException source target) target
eitherTryCast f s = case f s of
  Left e ->
    Left . TryCastException.TryCastException s . Just $ Exception.toException e
  Right t -> Right t

-- | This function is like 'TryCast.tryCast' except that it will throw an
-- impure exception if the conversion fails.
--
-- > -- Avoid this:
-- > either throw id . from
-- >
-- > -- Prefer this:
-- > unsafeCast
unsafeCast
  :: forall source target
   . ( Stack.HasCallStack
     , TryCast.TryCast source target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> target
unsafeCast = either Exception.throw id . TryCast.tryCast

-- | This function is like 'from' except that it will throw an impure
-- exception if the conversion fails.
--
-- > -- Avoid this:
-- > either throw id . from @s
-- >
-- > -- Prefer this:
-- > unsafeFrom @s
unsafeFrom
  :: forall source target
   . ( Stack.HasCallStack
     , TryCast.TryCast source target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> target
unsafeFrom = unsafeCast

-- | This function is like 'into' except that it will throw an impure
-- exception if the conversion fails.
--
-- > -- Avoid this:
-- > either throw id . into @t
-- >
-- > -- Prefer this:
-- > unsafeInto @t
unsafeInto
  :: forall target source
   . ( Stack.HasCallStack
     , TryCast.TryCast source target
     , Show source
     , Typeable.Typeable source
     , Typeable.Typeable target
     )
  => source
  -> target
unsafeInto = unsafeCast
