{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

-- |
--   Module     : Composite.Record.Tuple
--   License    : MIT
--   Stability  : experimental
--
-- Tuple functions for composite records, inspired by relude.
module Composite.Record.Tuple
  ( unit,
    singleton,
    pattern (:|:),
    toFst,
    toSnd,
    fmapToFst,
    fmapToSnd,
    traverseToFst,
    traverseToSnd,
    fanout,
    fanoutM,
  )
where

import Composite.Record

-- | Turn a unit into a record.
--
-- @since 0.1.3.0
unit :: () -> Rec f '[]
unit _ = RNil

-- | Put a single value in a record.
--
-- @since 0.1.1.0
singleton :: a -> Record (s :-> a : '[])
singleton a = a :*: RNil

-- | Pattern for a pair in a record
--
-- @since 0.1.2.0
pattern (:|:) :: a -> b -> Record (s :-> a : s' :-> b : '[])
pattern a :|: b = a :*: b :*: RNil

-- | Apply a function, with the result in the fst slot, and the value in the other.
--
-- @since 0.1.0.0
toFst :: (a -> b) -> a -> Record (s :-> b : s' :-> a : '[])
toFst f x = f x :*: x :*: RNil

-- | Apply a function with the result in the snd slot, and the value in the other.
--
-- @since 0.1.0.0
toSnd :: (a -> b) -> a -> Record (s :-> a : s' :-> b : '[])
toSnd f x = x :*: f x :*: RNil

-- | Like fmap, but also keep the original value in the snd position.
--
-- @since 0.1.0.0
fmapToFst :: Functor f => (a -> b) -> f a -> f (Record (s :-> b : s' :-> a : '[]))
fmapToFst = fmap . toFst

-- | Like fmap, but also keep the original value in the fst position.
--
-- @since 0.1.0.0
fmapToSnd :: Functor f => (a -> b) -> f a -> f (Record (s :-> a : s' :-> b : '[]))
fmapToSnd = fmap . toSnd

-- | Apply a function that returns a value inside of a functor, with the output in the first slot, the input in the second, and the entire tuple inside the functor.
--
-- @since 0.1.0.0
traverseToFst :: Functor m => (a -> m b) -> a -> m (Record (s :-> b : s' :-> a : '[]))
traverseToFst f x = (:*: x :*: RNil) <$> f x

-- | Apply a function that returns a value inside of a functor, with the output in the second slot, the input in the fist, and the entire tuple inside the functor.
--
-- @since 0.1.0.0
traverseToSnd :: Functor m => (a -> m b) -> a -> m (Record (s :-> a : s' :-> b : '[]))
traverseToSnd f x = (\y -> x :*: y :*: RNil) <$> f x

-- | Apply two functions to a single value and store the results in each slot.
--
-- @since 0.1.0.0
fanout :: (x -> a) -> (x -> b) -> x -> Record (s :-> a : s' :-> b : '[])
fanout f g x = f x :*: g x :*: RNil

-- | Apply two applicative functions to a single value and store the results in each slot.
--
-- @since 0.1.0.0
fanoutM :: Applicative m => (x -> m a) -> (x -> m b) -> x -> m (Record (s :-> a : s' :-> b : '[]))
fanoutM f g x = (\y z -> y :*: z :*: RNil) <$> f x <*> g x
