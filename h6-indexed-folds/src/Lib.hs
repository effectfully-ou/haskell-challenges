{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Lib
    ( Nat (..)
    , Vec (..)
    , ifoldl'
    ) where

data Nat = Z | S Nat

data Vec n a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a

deriving instance Foldable (Vec n)

ifoldr :: forall b n a. (forall m. a -> b m -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldr f z = go where
    go :: Vec m a -> b m
    go Nil         = z
    go (Cons x xs) = f x $ go xs
{-# INLINE ifoldr #-}

ifoldl' :: (forall m. b m -> a -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldl' _ _ = undefined
