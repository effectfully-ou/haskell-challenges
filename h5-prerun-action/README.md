# Prerun an action

This is a challenge to solve with Haskell. Check out the [readme of the whole project](../README.md) first if you haven't already.

Imagine you're given a function `f` of the following type: `IO a -> IO (IO b)`. We will refer

- to a value of type `IO a` as "the received action"
- to the outer/inner `IO` in `IO (IO b)` as "the outer/inner action" respectively.
- to a value of type `b` as "the final result"

It is known that `f` neither uses the received action within the outer action, nor returns it as a part of the final result. I.e. `f` uses the received action only within the inner action (any number of times, including zero). Here's one example of such a function:

```haskell
greetAndReturnPerformThrice :: Show a => IO a -> IO (IO ())
greetAndReturnPerformThrice a = do
    putStrLn "greetings"
    return . replicateM_ 3 $ a >>= print
```

`greetAndReturnPerformThrice` receives an `a :: IO a` and does not use it in the outer action (which only prints "greetings"), but `a` gets used in the inner action thrice with its result printed each time.

Your task is to go to [`src/Lib.hs`](./src/Lib.hs) and implement `prerun` that has the following type signature:

```haskell
prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
```

`prerun` "pushes" the lambda that binds the received action to the inside of the outer `IO` thus allowing to run the outer action before you can even provide the received action. For example:

```haskell
example :: IO ()
example = do
    performThrice <- prerun greetAndReturnPerformThrice  -- [1]
    performThrice getAllocationCounter                   -- [2]
    performThrice $ pure 42                              -- [3]
```

1. run the outer action of `greetAndReturnPerformThrice` (i.e. print "greetings") even though `greetAndReturnPerformThrice` hasn't been fed with a received action yet
2. instantiate the "pushed" function with some arbitrary computation (`getAllocationCounter` is a shitty way to generate a random `Int64` without depending on the `random` package) and run the resulting inner action
3. instantiate the "pushed" function again, this time with an action always returning a constant result, and run the resulting inner action

Output is expected to look something like that:

```
greetings
9223372036854543823
9223372036854528759
9223372036854513727
42
42
42
```

Notice how "greetings" is only printed once while `getAllocationCounter` and `pure 42` are executed thrice as they should (in particular, the result of `getAllocationCounter` differs every time, see the last digits).

Rules:

- performance does not matter at all. Feel free to make `prerun` as inefficient as you like (CPU-wise. Memory-wise you're limited by 64 megabytes)
- the inner action is allowed to use any variables bound in the outer action
- `prerun` should not add any extraneous strictness. If the inner action does not use the received action, the latter should not be forced. And if the inner action uses the received action, but the final result does not use the result of the received action, the latter should not be forced as well
- not only should it be possible to instantiate a function returned by `prerun` multiple times with different received actions, but it also should be possible to execute those instantiations in different threads concurrently (they don't have to actually run in parallel, feel free to block as you please as per the bullet about performance) without messing up which received action belongs to which instantiation
- moreover, async exceptions should be handled appropriately: an async exception thrown to an instantiation should reach the inner action, propagate upwards and not mess up any internal state potentially causing a deadlock

**UPDATE** Thanks to [**viercc**](https://github.com/viercc) and [**aaronallen8455**](https://github.com/aaronallen8455) there's now a hardcore mode of the challenge with one additional requirement: it should be possible to chain applications of a function returned by `prerun`. For example, this function:

```haskell
quadrice :: IO () -> IO ()
quadrice a = do
    twice <- prerun $ \a -> pure $ a *> a
    twice $ twice a
```

should execute its argument four times:

```
>>> quadrice $ putStrLn "I'm a line"
I'm a line
I'm a line
I'm a line
I'm a line
```

It should also be allowed to spawn a new thread from within `twice`'s argument, for example

```haskell
quadriceSpawn :: IO () -> IO ()
quadriceSpawn a = do
    twice <- prerun $ \a -> pure $ a *> a
    twice $ async (twice a) >>= wait
```

should behave equally to `quadrice`.

The hardcore mode is disabled by default, to enable it go to [test/Main.hs](./test/Main.hs) and replace `hardcore = False` with `hardcore = True`.

There's a small test suite. I run it with `stack test`.

This is an April Fools' Day edition where the fool is me who thought that it wouldn't be hard to write a test suite for this task: I've spent several days trying to find an optimal set of parameters that triggers as many errors as possible while reporting false positives with reasonably small probability (testing concurrent IO without mocking is no easy task) without the test suite taking minutes to run. If you're getting an error having "got a deadlock" in it, try increasing the `tIMEOUT` constant in [`test/Main.hs`](test/Main.hs) (if tests still fail, probably there's a problem indeed).
