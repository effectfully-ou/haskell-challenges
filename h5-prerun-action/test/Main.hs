module Main
    ( main
    ) where

import           Lib

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Data.IORef
import           Data.Ix
import           System.Random
import           System.Timeout

hardcore :: Bool
hardcore = False

tIMEOUT :: Int
tIMEOUT = 5000000

test_soundness :: IO ()
test_soundness = do
    outerVar <- newMVar (0 :: Int)
    innerVar <- newMVar (0 :: Int)
    f <- prerun $ \a -> do
        modifyMVar_ outerVar $ pure . succ
        pure a
    outer1 <- readMVar outerVar
    unless (outer1 == 1) $ fail "'prerun' did not execute the outer computation immediately"
    inner1 <- readMVar innerVar
    unless (inner1 == 0) $ fail "'prerun' executed the inner computation immediately"
    f . modifyMVar_ innerVar $ pure . succ
    outer2 <- readMVar outerVar
    unless (outer2 == 1) $ fail "'prerun' executed the outer computation again"
    inner2 <- readMVar innerVar
    unless (inner2 == 1) $ fail "'prerun' did not execute the inner computation"
    f mempty
    outer3 <- readMVar outerVar
    unless (outer3 == 1) $ fail "'prerun' executed the outer computation again"
    inner3 <- readMVar innerVar
    unless (inner3 == 1) $ fail "'prerun' did not update the inner computation"

test_strictness :: IO ()
test_strictness = do
    f1 <- prerun $ \a -> pure $ a *> pure ()
    f1 . pure $ error "The function forced the value of its argument despite not using it"
    f2 <- prerun $ \a -> pure $ replicateM_ 0 a
    f2 $ error "The function forced its argument despite not using it"

test_concurrency :: IO ()
test_concurrency = do
    let is0 = [0, 1, 2, 3, 10, 20, 30, 100, 200, 300]
    isVar <- newIORef is0
    caughtVar <- newMVar False
    replicateM_ (length is0) $ do
        may <- timeout tIMEOUT $ do
            let registerCaught = modifyMVar_ caughtVar $ \_ -> pure True
            f <- prerun $ \getN -> do
                i : is' <- readIORef isVar
                writeIORef isVar is'
                let d = (last is0 + 1) `div` (i + 1)
                pure $ replicateM i (getN <* threadDelay d) `onException` registerCaught
            let rans = take 100 $ zip (scanl (+) 1 [4 :: Int, 7..]) (scanl (+) 3 [6, 9..])
            threads <- forM rans $ \ran -> async $ do
                ns <- f $ randomRIO ran
                unless (all (inRange ran) ns) $
                    fail "The internal state of the inner computation was messed up in a different thread"
            for_ threads $ \thread -> do
                b <- randomIO
                if b
                    then wait thread
                    else do
                        let threadId = asyncThreadId thread
                        k <- randomRIO (0, 5)
                        replicateM_ k $ killThread threadId  -- DOMINATE
                        cancel thread
        case may of
            Nothing -> fail "Asynchronous exceptions were not handled correctly, got a deadlock"
            Just () -> pure ()
    caught <- readMVar caughtVar
    unless caught $ fail "No asynchronous exception has reached the inner computation"

test_reentrant :: IO ()
test_reentrant = do
    twice <- prerun $ \a -> pure $ (+) <$> a <*> a
    var <- newMVar 0
    let inc = modifyMVar var $ \n -> pure (succ n, n)
    res <- twice . twice $ async (twice $ twice inc) >>= wait
    unless (res == sum [0..15]) $ fail "Failed the hardcode mode"

main :: IO ()
main = do
    test_soundness
    test_strictness
    when hardcore $ test_reentrant
    test_concurrency
