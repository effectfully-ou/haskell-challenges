{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
module Lib
    ( forceElems
    ) where


import Data.Functor.Identity
import Control.Applicative

import Debug.Trace


-- data SN a = S !a | N a

data SemiStrictIdentity a = S !a | N a


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

instance Applicative SemiStrictIdentity where
    pure x = undefined -- Strict $ \() -> N x
    -- (<*>) :: f (a -> b) -> f a -> f b
    -- Работает с Tree2:
    (<*>) f b = N $ case f of
        N f' -> case b of
            S b' -> f' b'
            N b' -> f' b'
        S f' -> undefined

        --N f'  -> f' (case b () of
        --                S b' -> b'
        --                N b' -> b'
        --                )
        --S f' -> undefined

    --(<*>) (Strict f) (NonStrict b) = undefined
    --(<*>) (NonStrict f) (Strict b) = undefined -- NonStrict $ f b
    --(<*>) (NonStrict f) (NonStrict b) = undefined
    --
    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f left right = 
        --S $ f (case left () of
        --            S left' -> left'
        --            N left' -> left'
        --      )
        --      (case right () of
        --            S right' -> right'
        --            N right' -> right'
        --      )

        ---- Вариант, работающий со списками
        --case left () of
        --    S left' ->
        --        N $ f left' (case right () of
        --                        S right' -> right'
        --                        N right' -> right'
        --                        )
        --    N left' ->
        --        N $ f left' (case right () of
        --                        S right' -> right'
        --                        N right' -> right'
        --                    )


        -- Вариант, работающий с Tree2
        --case left of
        --    S left' -> undefined $ case right of
        --        S right' -> S $ f left' right'
        --        N right' -> N $ f left' right'
        --    --
        --    N left' -> case right of
        --        S right' -> N $ f left' right'
        --        N right' -> undefined $ N $ f left' right'

        -- Тоже работающий с Tree2 вариант
        case right of
            S right' -> N $ f (case left of
                S left' -> left'
                N left' -> left')
                right'
            N right' -> undefined {- case right of
                S right' -> N $ f left' right'
                N right' -> undefined $ N $ f left' right'
                -}


    --liftA2 f (NonStrict left) (Strict right) = undefined -- NonStrict $ f left $! right
    --liftA2 f (Strict left) (NonStrict right) = undefined
    --liftA2 f (NonStrict left) (NonStrict right) = undefined
  
instance Functor SemiStrictIdentity where
    {-# INLINE fmap  #-}
    fmap f a = undefined

runSemiStrictIdentity :: SemiStrictIdentity a -> a
runSemiStrictIdentity (S a) = a
runSemiStrictIdentity (N a) = a


forceElems :: Traversable t => t a -> t a
forceElems = runSemiStrictIdentity . traverse S -- Strict el)
-- forceElems = runSemiStrictIdentity . traverse (\(!el) -> Strict $ \() -> S el) -- Strict el)
-- forceElems = runIdentity . traverse (\el -> Identity el)


{-

-- newtype StrictIdentity a =  StrictIdentity {runStrictIdentity_ :: a }

-- | 'runStrictIdentity' unwraps a value of type  @'StrictIdentity' ty@  into a value of type @ty@,  strictly.
runStrictIdentity :: StrictIdentity a -> a 
runStrictIdentity ma = case runStrictIdentity_ $! ma of 
                            !res -> res
                            -- res -> res  
{-# INLINE  runStrictIdentity #-} 

instance Applicative StrictIdentity where
    -- {-# INLINE pure #-}
    pure = undefined -- StrictIdentity
    -- {-# INLINE (<*>) #-}
    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) (StrictIdentity f) (StrictIdentity b) = StrictIdentity $! f b
    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f (StrictIdentity left) (StrictIdentity !right) = StrictIdentity $! f left right
    -- {-# INLINE liftA2 #-}
  
instance Functor StrictIdentity where
    {-# INLINE fmap  #-}
    -- fmap f !m = StrictIdentity $! (f $! (runStrictIdentity m))
    fmap f m = undefined -- StrictIdentity (f (runStrictIdentity m))
  

-- для конструирования в функторе можно просто `x <$ fx`

data InfList a = InfList { v :: a, next :: InfList a } deriving (Show, Functor, Foldable, Traversable)


gen :: Int -> InfList Int
gen v = InfList v (gen $ v + 1)

takeInfList :: Int -> InfList a -> [a]
takeInfList 0 _ = []
takeInfList i (InfList v il) = v : takeInfList (i - 1) il

atInfList :: InfList a -> Int -> a
atInfList (InfList v _) 0 = v
atInfList (InfList _ next) i = atInfList next (i - 1)


forceElems :: Traversable t => t a -> t a
forceElems = runStrictIdentity . traverse (\(!el) -> StrictIdentity el)
-- forceElems = head . traverse (\(!el) -> [el])
-- forceElems = runIdentity . traverse (\(!el) -> Identity $! el)

-}


