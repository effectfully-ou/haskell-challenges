module Main
    ( main
    ) where

import           Lib

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Foldable
import           Data.Ix
import           System.Random
import           System.Timeout

tIMEOUT :: Int
tIMEOUT = 5000000

-- prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
-- f <- prerun $ \a -> a ... a ... a
-- f a1 ... f a2 ... f3

test_concurrency :: IO ()
test_concurrency =
    replicateM_ 6 $ do
        may <- timeout tIMEOUT $ do
            f <- prerun $ \getN -> do
                i <- randomRIO (0, 100)
                pure . replicateM i $ getN <* threadDelay 10
            let rans = take 100 $ zip (scanl (+) 1 [4 :: Int, 7..]) (scanl (+) 3 [6, 9..])
            threads <- forM rans $ \ran -> async $ do
                ns <- f $ randomRIO ran
                unless (all (inRange ran) ns) $
                    fail "The internal state of the function was messed up in a different thread"
            for_ threads $ \thread -> do
                b <- randomIO
                if b then cancel thread else wait thread
        case may of
            Nothing -> fail "Asynchronous exceptions were not handled correctly"
            Just () -> pure ()

main :: IO ()
main = test_concurrency
