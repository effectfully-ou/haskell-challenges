# h2-inspect-forced

You're given the following data type:

```haskell
data Tree
    = Leaf
    | Fork Tree Int Tree
```

Your task is to define a higher-order function in the [`src/Lib.hs`](src/Lib.hs) module with the following type signature:

```haskell
materializeForcedBy :: (Tree -> Int) -> Tree -> Tree
```

It takes two arguments, a function `f` of type `Tree -> Int` and a `Tree` `t` to apply that function to, and returns the subtree of `t` that evaluation of `f t` would force, replacing all unevaluated `Int` elements in the resulting subtree with `0`.

For example, having the following tree:

```
    3
   / \
  2   4
 /   / \
1   5   7
     \
      6
```

represented as


```haskell
someTree :: Tree
someTree =
    Fork
        (Fork
            (Fork Leaf 1 Leaf)
            2
            Leaf)
        3
        (Fork
            (Fork
                Leaf
                5
                (Fork Leaf 6 Leaf))
            4
            (Fork Leaf 7 Leaf))
```

and a function forcing a part of that tree:

```haskell
disturb :: Tree -> Int
disturb
    (Fork
        (Fork
            _
            _
            _)
        n3
        (Fork
            (Fork
                _
                n5
                _)
            _
            _)) = n3 + n5
```

a call to `materializeForcedBy disturb someTree` should return

```haskell
Fork (Fork Leaf 0 Leaf) 3 (Fork (Fork Leaf 5 Leaf) 0 Leaf)
```

which represents

```
    3
   / \
  0   0
     /
    5
```

Note that the `2` and `4` elements from the original tree got replaced by `0`s, because `disturb` does not use those elements for computing the result and so does not force them.

Conditions:

1. the function that `materializeForcedBy` receives always terminates and can force tree nodes and elements in any order
2. `materializeForcedBy` should be able to handle infinite trees
3. neither tree nodes nor elements can contain `error` calls, so feel free to force anything
4. performance is not a concern: feel free to introduce any computational or memory overhead, as long as it does not result in infinite loops (there's a hard cap of 256 MB for tests to consume though, just to prevent infinite looping and swapping, but that amount of memory should be enough for anyone)
5. it should be allowed for multiple calls to `materializeForcedBy` to run in parallel (possibly over the same tree)
