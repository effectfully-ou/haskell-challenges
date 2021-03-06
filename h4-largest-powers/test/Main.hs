{-# LANGUAGE BangPatterns #-}

module Main
    ( main
    ) where

import           Lib

import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

largestPower :: Int -> Int -> Int
largestPower n = go 0 where
    go !d m
        | r == 0    = go (d + 1) q
        | otherwise = d
        where (q, r) = m `quotRem` n

largestPowersIntDirect :: Int -> [Int]
largestPowersIntDirect n = map (largestPower n) [n, 2 * n..]

test_largestPowersSound :: TestTree
test_largestPowersSound =
    testProperty "sound" . withMaxSuccess 500 $ \n ->
        let n' = abs n + 2
        in take 2500 (largestPowersIntDirect n') === take 2500 (largestPowersInt n')

test_largestPowersEfficient :: TestTree
test_largestPowersEfficient =
    testCase "efficient" $
        foldl' (flip (-)) 0 (take (10^8) $ largestPowersInt 13) @?= -5

main :: IO ()
main =
    defaultMain $ testGroup "all"
        [ test_largestPowersSound
        , test_largestPowersEfficient
        ]
