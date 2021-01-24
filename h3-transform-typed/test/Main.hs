{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
    ( main
    ) where

import           Lib

import           Data.Bool
import           Data.List
import           Data.Typeable
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (Function)
import           Unsafe.Coerce

class ExtEq a where
    (====) :: a -> a -> Property

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => ExtEq a where
    (====) = (===)

instance {-# OVERLAPPING #-} (Arbitrary a, ExtEq b) => ExtEq (a -> b) where
    f ==== g = forAllBlind arbitrary $ \x -> f x ==== g x

instance {-# OVERLAPPABLE #-} Typeable a => Arbitrary (Scheme a) where
    arbitrary = pure $ Res Proxy

instance {-# OVERLAPPING #-}
        ( Typeable a, Typeable b, Arbitrary (Scheme b)
        ) => Arbitrary (Scheme (a -> b)) where
    arbitrary = frequency
        [ (1, pure $ Res Proxy)
        , (3, Arg Proxy <$> arbitrary)
        ]

collectTypesBy :: (forall a. Typeable a => Proxy a -> r) -> Scheme b -> [r]
collectTypesBy f (Res pr)     = [f pr]
collectTypesBy f (Arg pr sch) = f pr : collectTypesBy f sch

wrapProxy :: Proxy a -> Proxy (Wrap a)
wrapProxy Proxy = Proxy

test_function :: (Arbitrary (Scheme a), ExtEq a, Typeable a) => String -> a -> TestTree
test_function  name f =
    testProperty name . forAllBlind arbitrary $ \sch ->
        case wrapFunction $ Function sch f of
            Function sch' f' -> conjoin
                [ collectTypesBy (typeRep . wrapProxy) sch === collectTypesBy typeRep sch'
                , f ==== unsafeCoerce f'
                ]

main :: IO ()
main =
    defaultMain $ testGroup "all"
        [ test_function "zero"   (0 :: Int)
        , test_function "succ"   (succ @Int)
        , test_function "plus"   ((+) @Int)
        , test_function "true"   True
        , test_function "not"    not
        , test_function "and"    (&&)
        , test_function "bool"   (bool @())
        , test_function "nil"    ""
        , test_function "uncons" (uncons @())
        , test_function "take"   (take @Bool)
        , test_function "map"    (map @Char @Int)
        , test_function "zip4"   (zip4 @[()] @(Maybe Bool) @Int @Char)
        ]
