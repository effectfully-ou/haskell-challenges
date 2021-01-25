# Transform a function stored in an existential

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

You're given the following data type that reifies the type of a function at the term level:

```haskell
data Scheme a where
    Res :: Typeable a => Proxy a -> Scheme a
    Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)
```

For example, the most general `Scheme` for a two-argument function looks like this:

```haskell
twoArgs :: (Typeable a, Typeable b, Typeable c) => Scheme (a -> b -> c)
twoArgs = Arg Proxy . Arg Proxy $ Res Proxy
```

(`Typeable` is needed for testing and `Proxy` because otherwise dealing with `Scheme` is a pain in the ass).

Now given a data type that allows one to store any function together with its `Scheme`:

```haskell
data Function = forall a. Function (Scheme a) a
```

your task is to go to [`src/Lib.hs`](src/Lib.hs) and implement

```haskell
wrapFunction :: Function -> Function
```

that adds `Wrap` to the type of every reified argument of the underlying function, as well as to the type of its result. Where `Wrap` is a simple `newtype` wrapper:

```haskell
newtype Wrap a = Wrap
    { unWrap :: a
    }
```

For example, `wrapFunction` should turn

```haskell
plusFunction :: Function
plusFunction = Function twoArgs plusInt where
    plusInt :: Int -> Int -> Int
    plusInt = (+)
```

into an equivalent of

```haskell

plusFunctionWrapped :: Function
plusFunctionWrapped = Function twoArgs plusIntWrapped where
    plusIntWrapped :: Wrap Int -> Wrap Int -> Wrap Int
    plusIntWrapped (Wrap x) (Wrap y) = Wrap $ x + y
```

(note that `twoArgs` gets implicitly instantiated in these two cases in different ways)

Conditions:

1. you're not allowed to use any unsafe functions, in particular `unsafeCoerce`
2. don't worry about accidentally making underlying functions strict or laziness in general

There's a small test suite. I run it with `stack test`.
