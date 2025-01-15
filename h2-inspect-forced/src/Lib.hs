{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
module Lib
    ( Tree (..)
    , materializeForcedBy
    ) where


import Control.Monad
import Data.Functor
import Data.IORef
import System.IO.Unsafe


data Tree
    = Leaf
    | Fork Tree Int Tree
    deriving (Show, Eq)

materializeForcedBy :: (Tree -> Int) -> Tree -> Tree
materializeForcedBy disturber tree = unsafePerformIO $ do
    let !n = disturber (addTracers tree)
    traces <- readIORef tracesRef
    pure $ cutAtAll (map reverse traces) tree
  where
    !tracesRef = unsafePerformIO $ newIORef ([]::[[(Dir, Bool)]])
    addTracers = go []
      where
        go _ Leaf = Leaf
        go path (Fork left v right) =
            addTrace False path $
                Fork (go (L:path) left)
                     (addTrace True path v)
                     (go (R:path) right)
    addTrace keep path v =
        unsafePerformIO $ atomicModifyIORef' tracesRef (\lst -> (((L, keep) : map (,False) path):lst, ())) $> v


-- From test/Main.hs:

data Dir
    = L
    | R
    deriving (Show)


unite :: Tree -> Tree -> Tree
Leaf          `unite` tree2         = tree2
tree1         `unite` Leaf          = tree1
Fork l1 x1 r1 `unite` Fork l2 x2 r2 =
    Fork (l1 `unite` l2) (if x1 /= 0 then x1 else x2) (r1 `unite` r2)


cutAt :: [(Dir, Bool)] -> Tree -> Tree
cutAt []                   _            = Leaf
cutAt _                    Leaf         = Leaf
cutAt ((dir, keep) : path) (Fork l x r) =
    case dir of
        L -> Fork (cutAt path l) x' Leaf
        R -> Fork Leaf           x' (cutAt path r)
  where
    x' = if keep then x else 0

cutAtAll :: [[(Dir, Bool)]] -> Tree -> Tree
cutAtAll paths tree = foldr unite Leaf $ map (flip cutAt tree) paths

