# Indexed folds

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

Consider the type of length-indexed lists a.k.a. vectors:

```haskell
data Nat = Z | S Nat

data Vec n a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a
```

If you're not familiar with this kind of thing, check out [Fixed-Length Vector Types in Haskell](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html) or, if you're into video content, [How to program in types with length-indexed vectors: Part 1](https://www.youtube.com/watch?v=PHS3Q-tRjFQ&t=170s)

We can define an indexed [worker-wrapper](https://wiki.haskell.org/Worker_wrapper)-transformed right fold over this type:

```haskell
ifoldr :: forall b n a. (forall m. a -> b m -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldr f z = go where
    go :: Vec m a -> b m
    go Nil         = z
    go (Cons x xs) = f x $ go xs
```

Your task is to go to [`src/Lib.hs`](./src/Lib.hs) and implement a **strict** indexed **left** fold (the distinction between strict-left- and lazy-right-folding is [the usual one](https://en.wikipedia.org/wiki/Fold_(higher-order_function)))

```haskell
ifoldl' :: (forall m. b m -> a -> b ('S m)) -> b 'Z -> Vec n a -> b n
```

 **in terms of `ifoldr`**. I.e. you're only allowed to use `ifoldr` and not the constructors of `Vec` (`Nil` and `Cons`). You're also not allowed to use any kind of unsafety (`error`, `undefined`, non-termination, `unsafeCoerce`, `unsafePerformIO`, what have you). You can use any number of auxiliary definitions as long as none of them references the constructors of `Vec` (referencing `Vec` itself is fine) or uses any kind of unsafety. CPU performance does not matter, memory is capped at 8 MBs just to be generous.

There's a hardcore mode of the challenge that imposes an additional requirement: `ifoldl'` has to run in linear time. The hardcore mode is disabled by default, to enable it go to [test/Main.hs](./test/Main.hs) and replace `hardcore = False` with `hardcore = True`.

There's a small test suite. I run it with `stack test`.

If you enjoyed this or some other challenge and appreciate the effort or want to see answers, consider becoming a [sponsor of the project](https://github.com/sponsors/effectfully-ou).
