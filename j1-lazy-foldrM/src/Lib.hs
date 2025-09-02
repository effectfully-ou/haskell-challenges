module Lib
    ( foldrM
    ) where

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM = undefined
