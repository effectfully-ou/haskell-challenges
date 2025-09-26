{-# OPTIONS_GHC -O0 -fmax-simplifier-iterations=0 #-}

{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Lib               (foldrM)

import           Control.Exception
import           Control.Monad
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.HUnit

tIMEOUT :: Int
tIMEOUT = 10000000

data Nest a
    = Pure a
    | Nest (Nest a)

instance Functor Nest where
    fmap = liftM

instance Applicative Nest where
    pure = Pure
    (<*>) = ap

instance Monad Nest where
    m0 >>= f = go m0 where
        go (Pure x) = f x
        go (Nest m) = Nest $ go m

nestN :: Int -> a -> Nest a
nestN n0 x = go n0 where
    go 0 = Pure x
    go n = Nest . go $ n - 1

unNestN :: a -> Int -> Nest a -> a
unNestN z = go where
    go _ (Pure x) = x
    go 0 _        = z
    go n (Nest m) = go (n - 1) m

u :: a
u = error "this was not supposed to be forced"

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testCase "ignore all Nothing" $ foldrM (\_ _ -> Nothing) 0 Nothing  @?= Just @Int 0
    , testCase "ignore all Just"    $ foldrM (\_ _ -> Nothing) u (Just u) @?= Nothing @Int

    , testCase "ignore all nil"    $ foldrM (\_ _ -> Nothing) 0 []      @?= Just @Int 0
    , testCase "ignore all cons"   $ foldrM (\_ _ -> Nothing) u (u : u) @?= Nothing @Int
    , testCase "ignore el lazy"    $ foldrM (\_ z -> Left  z) 0 (u : u) @?= Left  @Int @Int 0
    , testCase "ignore el strict"  $ foldrM (\_ z -> Right z) 0 [u, u]  @?= Right @Int @Int 0
    , testCase "ignore acc lazy"   $ foldrM (\i _ -> Left  i) u [u, 2]  @?= Left  @Int @Int 2
    , testCase "ignore acc strict" $ foldrM (\i _ -> Right i) u [1, u]  @?= Right @Int @Int 1

    , testCase "correct order" $ foldrM enumFromTo 7 [1 .. 5 :: Int] @?= [1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6,1,2,1,2,3,1,2,1,2,3,1,2,3,4,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6,1,2,3,4,5,6,7]
    , testCase "short list" $ do
        rMay <- timeout tIMEOUT . evaluate . sum $ foldrM enumFromTo 13 [1 .. 13 :: Int]
        case rMay of
            Nothing -> assertFailure "too slow"
            Just r  -> r @?= 1931540

    , testCase "long list" $ do
        rMay <- timeout tIMEOUT . evaluate $
            foldrM (\i j k -> i + j + k) 42 [1 .. 150000 :: Int] 0
        case rMay of
            Nothing -> assertFailure "too slow"
            Just r  -> r @?= 11250075042

    , testCase "lazy free" $ do
        rMay <- timeout tIMEOUT . evaluate $
            unNestN 'b' 3000000 . foldrM (\_ -> nestN 100000) 'a' $ repeat u
        case rMay of
            Nothing -> assertFailure "too slow"
            Just r  -> r @?= 'b'
    ]
