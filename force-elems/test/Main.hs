{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Lib

import           Data.IORef
import           Data.List
import           System.IO.Unsafe
import           Test.HUnit

withTrace :: ((forall b. a -> b -> b) -> c) -> IO ([a], c)
withTrace k = do
    xsVar <- newIORef id
    let z = k $ \x y -> unsafePerformIO $ y <$ modifyIORef xsVar (. (x :))
    ds <- unsafeInterleaveIO $ readIORef xsVar
    return (ds [], z)

tracesAndNats :: IO ([Int], [()])
tracesAndNats =
    withTrace $ \trace ->
        let go n = trace n () : go (n + 1)
        in go 0

data Tree2 a
    = Fork2 (Tree2 a) a (Tree2 a) a
    deriving (Functor, Foldable, Traversable)

zigZag :: Tree2 a -> [a]
zigZag = go False where
    go False (Fork2 _ x r _) = x : go True  r
    go True  (Fork2 l _ _ y) = y : go False l

tracesAndPathsTree2 :: IO ([String], Tree2 ())
tracesAndPathsTree2 =
    withTrace $ \trace ->
        let go p =
              Fork2
                (go (p . ('l' :)))
                (trace (p "x") ())
                (go (p . ('r' :)))
                (trace (p "y") ())
        in go id

data Rose a
    = Rose a [Rose a]
    deriving (Functor, Foldable, Traversable)

rightmost :: Rose a -> [a]
rightmost (Rose x rs) = x : if null rs then [] else rightmost $ last rs

tracesAndPathsRose :: IO ([[Int]], Rose ())
tracesAndPathsRose =
    withTrace $ \trace ->
        let go n p =
              Rose
                (trace (p []) ())
                (map (\m -> go (n + 1) (p . (m :))) [0..n])
        in go 0 id

main :: IO ()
main = runTestTTAndExit . TestList $ map TestCase
    [ do
        (traces, nats) <- tracesAndNats
        length (take 6 nats) @?= 6
        traces @?= []
    , do
        (traces, nats) <- tracesAndNats
        length (take 1 $ forceElems nats) @?= 1
        traces @?= [0]
    , do
        (traces, nats) <- tracesAndNats
        length (take 5 $ forceElems nats) @?= 5
        traces @?= [0, 1, 2, 3, 4]
    , do
        (traces, paths) <- tracesAndPathsTree2
        length (take 3 $ zigZag paths) @?= 3
        traces @?= []
    , do
        (traces, paths) <- tracesAndPathsTree2
        length (take 4 . zigZag $ forceElems paths) @?= 4
        sort traces @?= ["rlrx", "rlry", "rlx", "rly", "rx", "ry", "x", "y"]
    , do
        (traces, paths) <- tracesAndPathsRose
        length (take 2 $ rightmost paths) @?= 2
        traces @?= []
    , do
        (traces, paths) <- tracesAndPathsRose
        length (take 7 . rightmost $ forceElems paths) @?= 7
        traces @?= inits [0..5]
    ]
