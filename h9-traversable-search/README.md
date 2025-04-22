# Broad search for any `Traversable`

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

`Data.Foldable` defines:

```haskell
-- | The 'find' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
```

This function doesn't terminate in the presence of infinite recursion (unless an element is found before infinite recursion is entered). For example given

```haskell
data Tree a
    = Leaf a
    | Fork (Tree a) a (Tree a)
    deriving (Functor, Foldable, Traversable)

inf :: Tree Int
inf = Fork inf 7 (Leaf 15)
```

`find (> 12)` is an infinite loop.

Your today's task is to define

```haskell
search :: Traversable t => (a -> Bool) -> t a -> Maybe a
```

such that `search (> 12)` finishes and returns `Just 15`.

Rules:

1. `search` should work for any kind of `DeriveTraversable`-derived `Traversable`, including a depth- and breadth-infinite `data Rose a = Rose a [Rose a]` or `newtype Matrix a = Matrix [[a]]`
2. `unsafePerformIO` and similar functions are not allowed
3. the implementation needs to be reasonably efficient
4. `search` can return any element satisfying the predicate, so feel free to process the given `Traversable` container in any order

There's a small test suite. I run it with `stack test` (takes about a second on my machine).
