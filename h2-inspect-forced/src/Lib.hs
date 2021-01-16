module Lib
    ( Tree (..)
    , materializeForcedBy
    ) where

data Tree
    = Leaf
    | Fork Tree Int Tree
    deriving (Show, Eq)

materializeForcedBy :: (Tree -> Int) -> Tree -> Tree
materializeForcedBy = undefined
