{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib
    ( Nat (..)
    , Vec (..)
    , ifoldl
    ) where

data Nat = Z | S Nat

data Vec n a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a

deriving instance Foldable (Vec n)

ifoldr :: (forall m. a -> b m -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldr f z Nil         = z
ifoldr f z (Cons x xs) = f x $ ifoldr f z xs

ifoldl :: (forall m. b m -> a -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldl _ _ = undefined
