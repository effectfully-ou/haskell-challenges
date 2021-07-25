# Largest powers

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

Consider a function `largestPower n m` that finds the largest `p` such that `n^p` is an exact divisor of `m` via straightforward iterated division:

```haskell
largestPower :: Int -> Int -> Int
largestPower n = go 0 where
    go !d m
        | r == 0    = go (d + 1) q
        | otherwise = d
        where (q, r) = m `quotRem` n
```

For example, `largestPower 2 120` equals `3`, because the largest `p` such that `2^p` is an exact divisor of `120` is `3` (`2^3` divides `120` while `2^4` doesn't).

Having `largestPower` we can define the following function returning an infinite sequence that consists of `p = largestPower n m` for all `m` such that `n` divides `m`:

```haskell
largestPowersIntDirect :: Int -> [Int]
largestPowersIntDirect n = ps where
    ms = [n, 2 * n..]
	ps = map (\m -> largestPower n m) ms
```

A couple of examples:

```
n: 2
ms: 2  4  6  8  10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 ...
ps: 1  2  1  3  1  2  1  4  1  2  1  3  1  2  1  5  1  2  1  3  1  2  1  4  1  2  1  ...

n: 3
ms: 3  6  9  12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 ...
ps: 1  1  2  1  1  2  1  1  3  1  1  2  1  1  2  1  1  3  1  1  2  1  1  2  1  1  4  ...
```

Clearly there's a pattern in the `ps` sequences and it should be possible to generate them directly without using iterated division. This is your task. Go to [`Lib.hs`](./src/Lib.hs) and implement

```haskell
largestPowers :: Iterable a => Int -> [a]
```

where `Iterable` is the following type class:

```haskell
class Eq a => Iterable a where
    zer :: a       -- 0
    inc :: a -> a  -- succ
    dec :: a -> a  -- pred
```

introduced solely to restrict the number of operations that you can use (note that you're allowed to use `(==)` over two `a`s due to the presence of the `Eq a` superclass).

Conditions:

1. the generated sequence must be infinite
2. generation must be fast enough for it to be possible to fold the first `10^8` elements with a function in a few seconds (if you have a slow machine, feel free to loosen this requirement)
3. you can't use more than 8 MB of RAM
4. you're allowed to tweak compilation flags as long as the previous condition is not violated
5. productivity should be greater than 85%

There's a small test suite. Running it via `stack test` gives me the following (automatically printed) stats (or better ones if I choose an uglier solution):

```
  11,587,917,768 bytes allocated in the heap
      82,317,248 bytes copied during GC
         478,944 bytes maximum residency (1392 sample(s))
          24,552 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1392 colls,     0 par    0.162s   0.164s     0.0001s    0.0006s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    1.814s  (  1.830s elapsed)
  GC      time    0.162s  (  0.164s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    1.976s  (  1.994s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    6,388,885,514 bytes per MUT second

  Productivity  91.8% of total user, 91.8% of total elapsed
```
