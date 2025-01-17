{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DeriveFunctor #-}

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

wrap :: (forall a b . Typeable a, Typeable b) => Scheme a -> Scheme (Wrap b)
wrap (Res v) = Res (Proxy :: Proxy b) -- $ Wrap <$> v
wrap (Arg a s) = Arg undefined undefined

wrapFunction :: Function -> Function
wrapFunction (Function (Res v) f)         = Function (Res $ Wrap <$> v) (Wrap f)
wrapFunction (Function (Arg arg sch) f) = Function (Arg Proxy (wrap sch)) (\(Wrap v) -> Wrap $ f v)
wrapFunction _ = undefined
