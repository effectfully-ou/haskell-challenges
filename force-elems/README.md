# `forceElems`

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

1. just like `forceElemsList`, `forceElems` does not force the spine of its argument (and so can handle infinite structures)
2. containers having constructors storing multiple elements of type `a` (the order in which those get forces does not matter) and multiple recursive occurrences are allowed, as well as polymorphic recursion
3. no need to handle weird custom `Traversable` instances that do not agree with `DeriveTraversable`, like `fmap id` calls inserted manually in the middle of a `traverse` definition (although that particular case can be handled)

Replace `forceElems = undefined` in [`src/Lib.hs`](src/Lib.hs) with an actual definition of `forceElems`.

There's a small test suite, which I run with `stack test` (`cabal` should probably work as well, but I haven't checked).
