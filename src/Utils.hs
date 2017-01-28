module Utils where

import Protolude

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = b >>= \x -> if x then t else f
