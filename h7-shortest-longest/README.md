# Shortest longest

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

Your today's task is to define a function returning all the shortest sublists among the longest subsublists in a list of list of lists. For example, in

```haskell
[ [ "a", "bc", "de", "f", "gh" ]
, [ "ijk", "lm", "nop", "q"]
, [ "rst" ]
, [ "uv", "w", "x", "yz"]
]
```

(`"<...>"` is a `String` which is a synonym for `[Char]`, which is how we have a list of list of lists here)

the longest (in their respective sublists) subsublists are

```haskell
[ [ "bc", "de", "gh" ]
, [ "ijk", "nop" ]
, [ "rst" ]
, [ "uv", "yz"]
]
```

among which and after concatenation the shortest sublists appearing in the exact order that they appear in the above list are

```haskell
["bc", "de", "gh", "uv", "yz"]
```

One way to define such a function is to retrieve all the longest subsublists, concatenate the results and find the shortest sublists among them like that:

```haskell
-- | Retrieve all lists of the minimum length among a list of lists.
shortest' :: [[a]] -> [[a]]
shortest' xss = filter (\xs -> length xs == m) xss where
    m = minimum $ map length xss

-- | Retrieve all lists of the maximum length among a list of lists.
longest' :: [[a]] -> [[a]]
longest' xss = filter (\xs -> length xs == m) xss where
    m = maximum $ map length xss

shortestLongest' :: [[[a]]] -> [[a]]
shortestLongest' = shortest' . concatMap longest'
```

However, what if some of the subsublists is infinite? Like in this case:

```haskell
[ [ "ab", "c", "de" ]
, [ "fgh", "ij" ]
, [ "jkl", repeat 'm', "no" ]
, [ "p", "qrst", "uv" ]
, [ "w", "x", "yz" ]
]
```

where we have `repeat 'm'` in the middle of the middle list. The longest subsublists are

```haskell
[ [ "ab", "de" ]
, [ "fgh" ]
, [ repeat 'm' ]
, [ "qrst" ]
, [ "yz" ]
]
```

among which and after concatenation the shortest ones are

```haskell
["ab", "de", "yz"]
```

We don't need to force `repeat 'm'` in its entirety to figure out that it's not the shortest list, but the above algorithm does attempt to do that, since it tries to calculate the length of each of the subsublists, which is not possible on infinite lists and so the algorithm diverges once it encounters `repeat 'm'`.

Note that if there are multiple infinite subsublists in a list, the effect of that is the same as if there was only one infinite subsublist, because such a list still does not contibute any finite sublists to choose the shortest ones from.

Your task is to go to [`src/Lib.hs`](./src/Lib.hs) and implement

```haskell
shortestLongest :: [[[a]]] -> [[a]]
```

that does not choke on infinite subsublists.

Rules:

1. assume that only subsublists (an arbitrary amount of them) can be infinite and not sublists and that there's always at least one sublist that consists only of finite subsublists (since if all longest subsublists are infinite, then there are no particular shortest sublists to return)
2. assume that no lists (neither subsublists, nor inner lists, nor the outer list) are empty (since handling corner cases is no fun and in this specific case they're particularly evil)
3. `shortestLongest` should be efficient enough for tests to run in under a few seconds and 8 MB of RAM
4. if there are multiple equal shortest sublists, all of them should be returned (i.e. you should not perform any deduplication)

There's a small test suite. I run it with `stack test`. If you run the tests and see errors mentioning `x(x)` for some `x`, that's how an infinite list of `x`s is printed.
