# `forceElems`

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

The following function "entangles" the elements of a list with its spine, so that whenever the outermost constructor gets forced, the head element also gets forced (unless the list is empty, in which case nothing extra happens):

```haskell
forceElemsList :: [a] -> [a]
forceElemsList = foldr ((:) $!) []
```

Check out [Trouble in paradise: Fibonacci](https://github.com/effectfully/sketches/tree/master/trouble-in-paradise-fibonacci) if you're interested how such a thing can be useful.

Note that `forceElemsList` does not force the spine of its argument, it only ensures that whenever some consumer forces the spine of the list returned by `forceElemsList`, the elements of that list get forced as well.

Can you define a function that does the same, but for any `Traversable`? I.e. a function having the following type signature:

```haskell
forceElems :: Traversable t => t a -> t a
```

Rules:

1. just like `forceElemsList`, `forceElems` does not force the spine of its argument (and so can handle infinite structures) _when there are elements of type `a` attached to it_ (if there are no elements of type `a`, feel free to force that spine until an element pops up)
2. containers having constructors storing multiple elements of type `a` (the order in which those get forces does not matter) and multiple recursive occurrences are allowed
3. no need to handle weird custom `Traversable` instances that do not agree with `DeriveTraversable` (like `fmap id` or `(pure id <*>)` calls inserted manually in the middle of the definition of `traverse`)
4. it is expected that `forceElems` introduces some performance overhead, even compared to `forceElemsList`

And there's a hardcore mode (turned off by default, change `hardcore = False` to `hardcore = True` in [`test/Main.hs`](src/Main.hs) to turn it on):

1. you're not allowed to force any non-`a` field (may occur in `f <*> pure x` for an `x` that is not of type `a`)
2. you're not allowed to force the spine any more than the final consumer forces it. I.e. "if there are no elements of type `a`, feel free to force that spine until an element pops up" is no longer allowed

Replace `forceElems = undefined` in [`src/Lib.hs`](src/Lib.hs) with an actual definition of `forceElems`.

There's a small test suite. I run it with `stack test`.
