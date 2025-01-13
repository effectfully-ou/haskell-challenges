{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
module Lib
    ( forceElems
    , forceElemsList
    ) where


import Data.Functor.Identity
import Control.Applicative

import Debug.Trace


-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b


forceElemsList :: [a] -> [a]
-- forceElemsList = foldr ((:) $!) []
forceElemsList = foldr (\a b -> ((:) $! a) $ b) []
                            --          ^ сначала будет WHNW этот элемент
                            --             ^ А вот тут уже без форса

-- data SN a = S !a | N a

-- data SemiStrictIdentity a = S !a | N a
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
    pure x = undefined -- Strict $ \() -> N x
    -- (<*>) :: f (a -> b) -> f a -> f b
    --(<*>) (NonStrict f) (Strict a) = undefined -- NonStrict $ f $! a
    --(<*>) (NonStrict f) (NonStrict a) = undefined -- NonStrict $ f a
    -- (<*>) (NonStrict _) _ = undefined -- NonStrict $ f a
    --(<*>) f' (Strict !a) = NonStrict $ (unNonStrict f') a {- case f' of
     --   NonStrict _ -> undefined
        -- Strict _ -> undefined -}
    -- (<*>) f' (NonStrict a) = NonStrict $ (unNonStrict f') a
    (<*>) f' a' = NonStrict $ case a' of Strict !a   -> case f' of NonStrict f -> f a
                                                                   _           -> undefined
                                         NonStrict a -> case f' of NonStrict f -> f a
                                                                   _           -> undefined

    --(<*>) (Strict f) (NonStrict b) = undefined
    --(<*>) (NonStrict f) (Strict b) = undefined -- NonStrict $ f b
    --(<*>) (NonStrict f) (NonStrict b) = undefined
    --
    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    -- liftA2 f (SSI left) right = error "xxx --- S?"
    -- liftA2 f (SSI left) right = SSI $ (f $! left) (unSSI right) -- работающий со списками вариант
    --liftA2 f (Strict !left)   (Strict    !right) = undefined -- NonStrict $ (f $! left) $! right
    --liftA2 f (NonStrict left) (Strict    !right) = undefined -- NonStrict $ f left $! right
    --liftA2 f (Strict !left)   (NonStrict right)  = undefined -- let !left' = left in NonStrict $ f left' right
    --liftA2 f (NonStrict left) (NonStrict right)  = undefined -- NonStrict $ f left right
    -- ??
    --liftA2 f left right = NonStrict $ case left of
    --    NonStrict left' -> case right of
    --                          Strict !right' -> f left' right'
    --                          NonStrict right' -> undefined
    --    Strict _ -> undefined
    liftA2 f left right = NonStrict $ case right of
        NonStrict right' -> case left of
                            NonStrict left' -> f left' right'
                            Strict !left' -> f left' right' 
        Strict !right' -> case left of
                            NonStrict left' -> f left' right'
                            Strict !left' -> f left' right'


    -- liftA2 f (N left) _ = error "xxx --- N?"
    {-
    liftA2 f (S left) (S right) = error "xxx --- SS"
    liftA2 f (S left) (N right) = error "xxx --- SN"
    liftA2 f (N left) (S right) = error "xxx --- NS"
    liftA2 f (N left) (N right) = error "xxx --- NN"
    -}
        {-
    liftA2 f left right = error "def"
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
                -}


    --liftA2 f (NonStrict left) (Strict right) = undefined -- NonStrict $ f left $! right
    --liftA2 f (Strict left) (NonStrict right) = undefined
    --liftA2 f (NonStrict left) (NonStrict right) = undefined
  
instance Functor SemiStrictIdentity where
    {-# INLINE fmap  #-}
    fmap f a = undefined

runSemiStrictIdentity :: SemiStrictIdentity a -> a
--runSemiStrictIdentity (S a) = a
-- runSemiStrictIdentity (N a) = a
runSemiStrictIdentity (Strict a) = a
runSemiStrictIdentity (NonStrict a) = a


forceElems :: Traversable t => t a -> t a
forceElems = runSemiStrictIdentity . traverse Strict

{-


newtype StrictIdentity a =  StrictIdentity {runStrictIdentity_ :: a }

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
    (<*>) (StrictIdentity f) (StrictIdentity b) = error "HERE <*>" -- StrictIdentity $! f b
    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f (StrictIdentity left) (StrictIdentity !right) = error "liftA2" -- StrictIdentity $! f left right
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


-- forceElems :: Traversable t => t a -> t a
-- forceElems = runStrictIdentity . traverse (\(!el) -> let z = StrictIdentity $! el)
-- forceElems = runStrictIdentity . traverse (StrictIdentity $!)
-- forceElems = head . traverse (\el -> (($!) (:)) el [])
-- forceElems = id
-- forceElems = head . traverse (\(!el) -> [el])
-- forceElems = runIdentity . traverse (\(!el) -> Identity $! el)


 -}
