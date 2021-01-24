{-# LANGUAGE GADTs #-}

module Lib where

import           Data.Proxy
import           Data.Typeable

data Scheme a where
    Res :: Typeable a => Proxy a -> Scheme a
    Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)

data Function = forall a. Function (Scheme a) a

newtype Wrap a = Wrap
    { unWrap :: a
    }

wrapFunction :: Function -> Function
wrapFunction = undefined
