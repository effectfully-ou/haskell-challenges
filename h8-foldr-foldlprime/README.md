# `foldr` via `foldl'`

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

It is well-known that `foldl'` can be and [is](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Foldable.html#foldl%27) defined in terms of `foldr` (even though it's a [harder thing](https://github.com/effectfully-ou/haskell-challenges/tree/master/h6-indexed-folds) to do when there are indices involved). Your today's task is to define `foldr` in terms of `foldl'`. That is, go to [src/Lib.hs](./src/Lib.hs) and define the properly lazy right fold

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

in terms of the strict left fold imported from `Data.Foldable`.

Rules:

1. you're not allowed to change the type signature of `foldr`
2. you're not allowed to use any other `Foldable`-related function apart from `foldl'`
3. you can use anything and everything else
4. your implementation must be as lazy as the original `foldr`. For example, `foldr (\_ _ -> True) False (unsafePerformIO launchMissiles : unsafePerformIO launchMissiles)` must never cause `launchMissiles` to be executed
5. the implementation must be fast enough for `foldr` to be able to handle a list with `10^6` elements
6. do feel free to change runtime options in the `.cabal` file (apart from the `-M32M` one) if you need that
7. don't worry about things being garbage-collected properly or asynchronous exceptions

There's a small test suite. I run it with `stack test`.
