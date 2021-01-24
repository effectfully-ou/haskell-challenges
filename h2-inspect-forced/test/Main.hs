module Main
    ( main
    ) where

import           Lib

import           Test.Tasty
import           Test.Tasty.QuickCheck

sumAt :: [(Dir, Bool)] -> Tree -> Int
sumAt []                   _            = 0
sumAt _                    Leaf         = 0
sumAt ((dir, keep) : path) (Fork l x r) =
    x' + case dir of
        L -> sumAt path l
        R -> sumAt path r
  where
    x' = if keep then x else 0

sumAtAll :: [[(Dir, Bool)]] -> Tree -> Int
sumAtAll paths tree = sum $ map (flip sumAt tree) paths

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

data Dir
    = L
    | R
    deriving (Show)

instance Arbitrary Dir where
    arbitrary = elements [L, R]

infiniteTree :: Tree
infiniteTree = go 0 where
    go n = Fork (go $ n - 1) n (go $ n + 1)

instance Arbitrary Tree where
    arbitrary = sized go where
        go n = frequency
            [ (1, elements $ Leaf : if n > 8 then [infiniteTree] else [])
            , (n, Fork <$> go (n `div` 2) <*> arbitrary <*> go (n `div` 2))
            ]

test_materializeForcedBy :: String -> TestTree
test_materializeForcedBy name =
    testProperty name . withMaxSuccess 300 $ \paths tree ->
        materializeForcedBy (sumAtAll paths) tree === cutAtAll paths tree

main :: IO ()
main =
    defaultMain . testGroup "all" $
        map (test_materializeForcedBy . show) [1 :: Int .. 10]
