{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Lib

import           Control.Monad
import           Data.IORef
import           Prelude               hiding (foldl', foldr, head, last,
                                        length, map, null, take)
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

head :: Foldable t => t a -> a
head = foldr const $ error "empty list"

last :: Foldable t => t a -> a
last xs = foldr (\x r _ -> r x) id xs $ error "empty list"

take :: Int -> [a] -> [a]
take n xs = foldr step (const []) xs n where
    step x r n
        | n <= 0    = []
        | otherwise = x : r (n - 1)

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' f z xs = foldr (\x r a -> r $! f a x) id xs z

length :: Foldable t => t a -> Int
length = foldl' (const . succ) 0

test_recons :: TestTree
test_recons =
    testProperty "foldr (:) [] === id" . withMaxSuccess 1000 $
        \(xs :: [Int]) -> foldr (:) [] xs == xs

test_take :: TestTree
test_take =
    testProperty "take n . repeat === replicate n" . withMaxSuccess 1000 $
        \n (i :: Int) -> take n (repeat i) == replicate n i

test_takeRecons :: TestTree
test_takeRecons =
    testProperty "take n . foldr (:) [] . repeat === replicate n" . withMaxSuccess 1000 $
        \n (i :: Int) -> take n (foldr (:) [] $ repeat i) == replicate n i

withNoForce :: ((forall a. a -> a) -> IO ()) -> IO ()
withNoForce k = do
    isForcedVar <- newIORef False
    k $ \x -> unsafePerformIO $ x <$ writeIORef isForcedVar True
    isForced <- readIORef isForcedVar
    when isForced $ assertFailure "missiles were launched"

test_null :: TestTree
test_null =
    testCase "null" $
        withNoForce $ \noForce -> do
            null "" @?= True
            null (noForce 'a' : noForce "") @?= False

test_head :: TestTree
test_head =
    testCase "head" $
        withNoForce $ \noForce ->
            head ('a' : noForce "") @?= 'a'

test_length :: TestTree
test_length =
    testCase "length" $
        withNoForce $ \noForce -> do
            length (map noForce "") @?= 0
            length (map noForce "abcd") @?= 4

test_fastEnough :: TestTree
test_fastEnough =
    testCase "fastEnough" $
        last [1..10^6] @?= 10^6

main :: IO ()
main =
    defaultMain . testGroup "all" $
        [ test_recons
        , test_take
        , test_takeRecons
        , test_null
        , test_head
        , test_length
        , test_fastEnough
        ]
