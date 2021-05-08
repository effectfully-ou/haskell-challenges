{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main
    ( main
    ) where

import           Lib

import           Control.Exception
import           Data.Foldable
import           Data.Functor.Const
import           Data.Maybe
import           Data.Some.Newtype
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

hardcore :: Bool
hardcore = False

newtype Flip f y x = Flip
    { unFlip :: f x y
    }

-- Generates the vector lazily.
listToVec :: [a] -> Some (Flip Vec a)
listToVec []     = Some $ Flip Nil
listToVec (x:xs) = case listToVec xs of
    Some (Flip rs) -> Some . Flip $ Cons x rs

withVec :: [a] -> (forall n. Vec n a -> r) -> r
withVec xs k = case listToVec xs of
    Some (Flip rs) -> k rs

reverseVec :: Vec n a -> Vec n a
-- Just for the shits and giggles of it.
reverseVec = unFlip . ifoldl' ((Flip .) . flip Cons . unFlip) (Flip Nil)

test_ifoldl'_sound :: TestTree
test_ifoldl'_sound =
    testProperty "ifoldl' is sound" . withMaxSuccess 1000 $ \(xs :: [Int]) ->
        withVec xs (toList . reverseVec) == reverse xs

data Stop = Stop
    deriving (Show, Exception)

test_ifoldl'_strict :: TestTree
test_ifoldl'_strict =
    testCase "ifoldl' is strict" $ do
        mayStopped <- timeout 500000 $ do
            let body = getConst . ifoldl' (\(Const acc) n -> Const $ acc + n) (throw Stop)
            try @Stop . evaluate $ withVec [1..] body
        isJust mayStopped @?= True

test_ifoldl'_linear :: TestTree
test_ifoldl'_linear =
    testCase "ifoldl' is linear" $ do
        let n = 10^7 :: Integer
        mayRes <- timeout 30000000 $ do
            let body = getConst . ifoldl' (\(Const acc) n -> Const $ n - acc) (Const 0)
            evaluate $ withVec [1..n] body
        mayRes @?= Just (n `div` 2)

main :: IO ()
main =
    defaultMain . testGroup "all" $
        [ test_ifoldl'_sound
        , test_ifoldl'_strict
        ] ++ [test_ifoldl'_linear | hardcore]
