{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main
    ( main
    ) where

import           Lib

import           Data.Char
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NonEmpty
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Debug.Trace

est' :: ([Int] -> Int) -> [[a]] -> [[a]]
est' pick xss = filter (\xs -> length xs == m) xss where
    m = pick $ map length xss

shortest' :: [[a]] -> [[a]]
shortest' = est' minimum

longest' :: [[a]] -> [[a]]
longest' = est' maximum

shortestLongest' :: [[[a]]] -> [[a]]
shortestLongest' = shortest' . concatMap longest'

data ListKind
    = Finite (NonEmpty Char)
    | Infinite Char
    deriving (Show)

newtype Short a = Short
    { unShort :: a
    } deriving stock (Functor)

newtype Input = Input
    { unInput :: Short (NonEmpty (Short (NonEmpty ListKind)))
    } deriving newtype (Arbitrary)

instance Arbitrary a => Arbitrary (Short (NonEmpty a)) where
    arbitrary =
        fmap Short . sized $ \n ->
            (:|) <$> arbitrary <*> listUpTo (n `rem` 10 + n `div` 10) arbitrary

    shrink (Short xs) = map Short $ shrink xs

runListKind :: ListKind -> String
runListKind (Finite xs)  = NonEmpty.toList xs
runListKind (Infinite x) = repeat x

runInputWith :: (ListKind -> String) -> NonEmpty (Short (NonEmpty ListKind)) -> [[String]]
runInputWith f = map (map f . NonEmpty.toList . unShort) . NonEmpty.toList

runInput :: NonEmpty (Short (NonEmpty ListKind)) -> [[String]]
runInput = runInputWith runListKind

instance Show Input where
    show = show . runInputWith f . unShort . unInput where
        f (Finite s)   = NonEmpty.toList s
        f (Infinite c) = c : '(' : c : ")"

listUpTo :: Int -> Gen a -> Gen [a]
listUpTo m gen = do
    n <- choose (0, m)
    vectorOf n gen

-- Em, QuickCheck?
instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

    shrink (x :| xs) = map (uncurry (:|)) $ shrink (x, xs)

arbitraryAlphaNumChar :: Gen Char
arbitraryAlphaNumChar = arbitrary `suchThat` \c -> any ($ c) [isLower, isUpper, isDigit]

instance Arbitrary ListKind where
    arbitrary = frequency
        [ (4, Finite <$> ((:|) <$> arbitraryAlphaNumChar <*> listUpTo 9 arbitraryAlphaNumChar))
        , (1, Infinite <$> arbitraryAlphaNumChar)
        ]

    shrink (Finite xs)  = Finite <$> shrink xs
    shrink (Infinite i) = pure . Finite $ i :| replicate 10 i

cutInfinite :: ListKind -> ListKind
cutInfinite (Finite xs)  = Finite xs
cutInfinite (Infinite i) = Finite $ i :| replicate 10 i

isFinite :: ListKind -> Bool
isFinite (Finite _)   = True
isFinite (Infinite _) = False

test_basic :: TestTree
test_basic =
    testCase "basic" $ shortestLongest xsss @?= ["bc", "de", "gh", "uv", "yz"] where
        xsss =
            [ [ "a", "bc", "de", "f", "gh" ]
            , [ "ijk", "lm", "nop", "q"]
            , [ "rst" ]
            , [ "uv", "w", "x", "yz"]
            ]

test_infinite1 :: TestTree
test_infinite1 =
    testCase "infinite 1" $ shortestLongest xsss @?= ["ab", "de", "yz"] where
        xsss =
            [ [ "ab", "c", "de" ]
            , [ "fgh", "ij" ]
            , [ "jkl", repeat 'm', "no" ]
            , [ "p", "qrst", "uv" ]
            , [ "w", "x", "yz" ]
            ]

test_infinite2 :: TestTree
test_infinite2 =
    testCase "infinite 2" $ shortestLongest xsss @?= ["ab", "de", "yz"] where
        xsss =
            [ [ "ab", "c", "de" ]
            , [ cycle "fgh", "ij" ]
            , [ cycle "jkl", repeat 'm', cycle "no" ]
            , [ "p", cycle "qrst", "uv" ]
            , [ "w", "x", "yz" ]
            ]

test_cases :: TestTree
test_cases =
    testGroup "cases"
        [ test_basic
        , test_infinite1
        , test_infinite2
        ]

test_arbitrary :: TestTree
test_arbitrary =
    testProperty "arbitrary" $ \(Input (Short xsss)) -> withMaxSuccess 5000 $ do
        let expected = shortestLongest' . runInput $ fmap (fmap (fmap cutInfinite)) xsss
            actual = shortestLongest $ runInput xsss
        any (all isFinite . unShort) xsss ==> actual === expected

main :: IO ()
main =
    defaultMain . testGroup "all" $
        [ test_cases
        , test_arbitrary
        ]
