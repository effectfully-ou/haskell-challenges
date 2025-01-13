{-# LANGUAGE BangPatterns #-}
module Lib
    ( forceElems
    , forceElemsList
    ) where


import Data.Functor.Identity
import Control.Applicative


forceElemsList :: [a] -> [a]
-- forceElemsList = foldr ((:) $!) []
forceElemsList = foldr (\a b -> ((:) $! a) $ b) []
                            --          ^ сначала будет WHNW этот элемент
                            --             ^ А вот тут уже без форса

data SemiStrictIdentity a = Strict a
                          | NonStrict { unNonStrict :: a }

{-

data Tree2 a
    = Fork2 (Tree2 a) a (Tree2 a) a
    deriving (Functor, Foldable, Traversable)

instance Traversable Tree2 where
    traverse f (Fork2 tl x tr y)
      = (<*>)

          ((<*>)
             (liftA2 (\b1 b2 b3 b4 -> Fork2 b1 b2 b3 b4)
                     (traverse f tl)
                     (f x)           -- <- применение к x
             )
             (traverse f tr)
          )

          (f y)                      -- <- применение к y




instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys

foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

foldr _ z []     =  z
foldr f z (x:xs) =  f x (foldr f z xs)
-}


-- На данный момент получается, что надо как-то форсить **элементы** дерева,
-- но не форсить поддеревья. Т.е. вот такой код
--
-- treeForce (Fork2 l x r y) = (((Fork2 $! (treeForce l)) $! x) (treeForce r)) $! y
--                                      ^^
--
-- уже не работает.
--
-- Кажется, это можно достигнуть, введя два типа элементов в структуре:
-- строгие и нестрогие и тогда уже оперировать именно ими


instance Applicative SemiStrictIdentity where
    pure = Strict

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) f' a' = NonStrict $ case a' of Strict !a   -> case f' of NonStrict f -> f a
                                                                   _           -> undefined
                                         NonStrict a -> case f' of NonStrict f -> f a
                                                                   _           -> undefined

    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f left right = NonStrict $ case right of
        NonStrict right' -> case left of
                                NonStrict  left' -> f left' right'
                                Strict    !left' -> f left' right' 
        Strict !right' -> case left of
                                NonStrict  left' -> f left' right'
                                Strict    !left' -> f left' right'


instance Functor SemiStrictIdentity where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f a = NonStrict $ case a of NonStrict  a' -> f a'
                                     Strict    !a' -> f a'


runSemiStrictIdentity :: SemiStrictIdentity a -> a
runSemiStrictIdentity (Strict a)    = a
runSemiStrictIdentity (NonStrict a) = a


forceElems :: Traversable t => t a -> t a
forceElems = runSemiStrictIdentity . traverse Strict

