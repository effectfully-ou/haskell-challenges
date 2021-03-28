module Lib
    ( prerun
    ) where

prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun = undefined
