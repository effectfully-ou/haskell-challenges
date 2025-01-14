{-# LANGUAGE BangPatterns #-}
module Lib
    ( forceElems
    ) where


import Data.Functor.Identity
import Control.Applicative


data SemiForcedIdentity a = Forced a
                          | NonForced a


instance Applicative SemiForcedIdentity where
    pure = NonForced

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) f' a' = NonForced $ case a' of
        Forced  a   ->
            case f' of NonForced f -> f $! a
                       _           -> undefined
        NonForced a ->
            case f' of NonForced f -> f a
                       _           -> undefined

    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f left right = NonForced $ case right of
        NonForced right' ->
            case left of NonForced  left' -> f left' right'
                         Forced    !left' -> f left' right'
        Forced  right' ->
            case left of NonForced  left' -> f left' $! right'
                         Forced     left' -> let !l = left' in (f l $! right')


instance Functor SemiForcedIdentity where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f a = NonForced $ case a of NonForced  a' -> f a'
                                     Forced    !a' -> f a'


runSemiForcedIdentity :: SemiForcedIdentity a -> a
runSemiForcedIdentity (Forced a)    = a
runSemiForcedIdentity (NonForced a) = a


forceElems :: Traversable t => t a -> t a
forceElems = runSemiForcedIdentity . traverse Forced


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
                     (f x)
             )
             (traverse f tr)
          )

          (f y)




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


forceElemsList :: [a] -> [a]
-- forceElemsList = foldr ((:) $!) []
forceElemsList = foldr (\a b -> ((:) $! a) $ b) []
                            --          ^ first this WHNW will be calculated
                            --             ^ and then this without force

-}
