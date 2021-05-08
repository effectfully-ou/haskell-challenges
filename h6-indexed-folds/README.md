# Indexed folds

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

**You're encouraged to post a solution to the challenge, but please refrain from doing so within 24 hours (give or take a few hours) after the challenge gets published, so that more people are motivated to participate. When you post a solution please do that via a link rather than inline code in order not to spoil the fun for others.**

Consider the type of length-indexed lists a.k.a. vectors:

```haskell
data Nat = Z | S Nat

data Vec n a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a
```

If you're not familiar with this thing, check out [Fixed-Length Vector Types in Haskell](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html) or, if you're into video content, [How to program in types with length-indexed vectors: Part 1](https://www.youtube.com/watch?v=PHS3Q-tRjFQ&t=170s)

We can define an indexed right fold over this type:

```haskell
ifoldr :: (forall m. a -> b m -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldr f z Nil         = z
ifoldr f z (Cons x xs) = f x $ ifoldr f z xs
```

Your task is to go to [`src/Lib.hs`](./src/Lib.hs) and implement an indexed left fold (the distinction between left- and right-folding is [the usual one](https://en.wikipedia.org/wiki/Fold_(higher-order_function)))

```haskell
ifoldl :: (forall m. b m -> a -> b ('S m)) -> b 'Z -> Vec n a -> b n
```

 **in terms of `ifoldr`**. I.e. you're only allowed to use `ifoldr` and not the constructors of `Vec` (`Nil` and `Cons`). You're also not allowed to use any kind of unsafety (`error`, `undefined`, non-termination, `unsafeCoerce`, `unsafePerformIO`, what have you).

There's a small test suite. I run it with `stack test`.

If you enjoyed this or some other challenge and appreciate the effort or want to see answers, consider becoming a [sponsor of the project](https://github.com/sponsors/effectfully-ou).
