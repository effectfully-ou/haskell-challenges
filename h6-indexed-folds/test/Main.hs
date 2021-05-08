{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Main
    ( main
    ) where

import           Lib

import           Data.Foldable
import           Test.QuickCheck

withVec :: [a] -> (forall n. Vec n a -> r) -> r
withVec []     k = k Nil
withVec (x:xs) k = withVec xs $ k . Cons x

newtype Flip f y x = Flip
    { unFlip :: f x y
    }

reverseVec :: Vec n a -> Vec n a
-- Just for the shits and giggles of it.
reverseVec = unFlip . ifoldl ((Flip .) . flip Cons . unFlip) (Flip Nil)

prop_reverse :: [Int] -> Bool
prop_reverse xs = withVec xs (toList . reverseVec) == reverse xs

main :: IO ()
main = quickCheck prop_reverse
