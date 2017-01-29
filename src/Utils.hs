module Utils where

import Protolude

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z
