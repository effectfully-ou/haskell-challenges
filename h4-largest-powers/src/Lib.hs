module Lib
    ( largestPowersInt
    ) where

import           Data.List
import           Data.Semigroup

class Eq a => Iterable a where
    zer :: a
    inc :: a -> a
    dec :: a -> a

instance Iterable Int where
    zer = 0
    inc = succ
    dec = pred

largestPowersInt :: Int -> [Int]
largestPowersInt = largestPowers

largestPowers :: Iterable a => Int -> [a]
largestPowers = undefined
