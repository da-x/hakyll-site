---
excerpt: Haskell
title: Performance of the ST Monad with pure exceptions
published: 2016-02-20
author: dan
---

The [ST Monad](https://wiki.haskell.org/Monad/ST) provides a venerable method in Haskell for writing stateful imperative code. Writing such code, in contrast to the non-stateful approach, is sometimes better. Some algorithms are better understood or better illustrated with states, and another reason is increased performance. The difference between `ST` and `IO` is important, because when we implement an algorithm, we only want to deal with the internal states and not bother with side effects that don't belong to it. Allowing stateful algorithm to remain pure under `ST`, gives way to better code generation by the compiler.

Some algorithms are better written with [Exceptions](https://wiki.haskell.org/Exception). For example, an algorithm for validating an expression tree may be such one. However, one needs to be aware that exceptions in pure code can only be caught in `IO`, unless [pure exceptions](https://hackage.haskell.org/package/exceptions) are used. We can use these pure exceptions under a [monad transformer](http://book.realworldhaskell.org/read/monad-transformers.html), but then we need to verify that there was no significant loss in performance. We suspect that the `CatchT` transformer would provide us a zero-cost abstraction, being a `newtype`. But how well would GHC succeed in optimizing away the transformations?

### Fibonacci that throws, for kicks

First, let us look at the example for `ST` brought from the [Haskell Wiki](https://wiki.haskell.org/Monad/ST), which presents us with the stateful implementation of computing the n-th Fibonacci number:

~~~~ {.haskell fancydiff=1 }
fibST :: Integer -> Integer
fibST n =
    if n < 2
        then n
        else runST $ do x <- newSTRef 0
                        y <- newSTRef 1
                        fibST' n x y

    where fibST' 0 x _ = readSTRef x
          fibST' n' x y = do
              x' <- readSTRef x
              y' <- readSTRef y
              writeSTRef x y'
              writeSTRef y $! x'+y'
              fibST' (n' - 1) x y
~~~~

We would like to test the performance of pure exceptions. So, let us have a slightly modified version of it, being a modulo of Fibonacci using `Int`. The change of type from `Integer` to `Int` would be better for us when measuring performance, otherwise the run would have spent time adding really big numbers in each one of the loop iterations.

We will use this new `Exception` type,

~~~~ {.haskell fancydiff=1 }
data MyException = MyException Int
  deriving Show
instance Exception MyException
~~~~

... and modify the function in various ways:

* Have the caller use `runST`.
* Change the type signature bear `Int` and be under `ST`.
* Add a throw to some exception along the way.

~~~~ {.haskell fancydiff=1 mark1Start="{*\ " mark1End="\ *}" mark1Class=sourceMarker  }
{* fibMod  :: Int -> ST s Int *}
{* fibMod n = do *}
    if n < 2
       then {* return n *}
       else do x <- newSTRef 0
               y <- newSTRef 1
               fibMod' n x y

    where
        fibMod' 0 x _ = readSTRef x
        fibMod' n' x y = do
            x' <- readSTRef x
            y' <- readSTRef y
            {* when (n' == 1000) $ do *}
                {* throw $ MyException x' -- not a pure exception (yet!) *}
            writeSTRef x y'
            writeSTRef y $! x'+y'
            fibMod' (n'-1) x y
~~~~

We have yet to add a `catch` anywhere. The problem is that we cannot have a pure function in `ST` doing `catch`, because `catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a`. If we try to use `catch`, we will get the error `No instance for (MonadCatch (ST s))`.

To solve, we shall use the `ST` monad with `CatchT`. First, we define `STCatch` type synonym for a short hand:

~~~~ {.haskell fancydiff=1 }
type STCatch s a = CatchT (ST s) a
~~~~
Now, let us create our `fibMod_E` variant, which is under `STCatch`, and modify it to use pure exceptions, thrown using `throwM`. We will also add a `catch` wrap, which will fix `fibMod_E` to return `-1` on the thrown exception. The `catch` is conditional, so we can see its effect depending on the use.

~~~~ {.haskell fancydiff=1 mark1Start="{*\ " mark1End="\ *}" mark1Class=sourceMarker  }
fibMod_E :: Int -> {* STCatch *} s Int
fibMod_E n =
    if n < 2
       then return n
       else do x <- {* lift $ *} newSTRef 0
               y <- {* lift $ *} newSTRef 1
               fibMod' n x y

    where
        fibMod' 0 x _ = {* lift $ *} readSTRef x
        fibMod' n' x y = do
            x' <- {* lift $ *} readSTRef x
            y' <- {* lift $ *} readSTRef y
            {* lift $ *} writeSTRef x y'
            {* lift $ *} writeSTRef y $! x'+y'
            when (abs n' == 1000) $ do
                {* throwM *} $ MyException x'

            {* let recurse = *} fibMod' (n'-1) x y
            {* if n <= 25000000 *}
               {* then recurse *}
               {* else catch recurse (\(MyException _) -> return (-1)) *}

~~~~

Can we degrade an `ST` exception back to an `IO` exception? Yes! Using the following function, that requires the `RankNTypes` extension for its type signature:

~~~~ {.haskell fancydiff=1 }
-- | A variant of `runST` for STCatch that turns all _uncaught_
-- 'throwM' exceptions back to exceptions thrown in `IO`.
runSTthrowIO :: (forall s. STCatch s a) -> a
runSTthrowIO action =
    case runST $ runCatchT action of
        Left e -> throw e
        Right r -> r
~~~~

### Testing

Now we are ready for testing. We will use the following utility, depending on [criterion](https://hackage.haskell.org/package/criterion):

~~~~ {.haskell fancydiff=1 }
timeIt :: IO () -> IO ()
timeIt act = do
    let w'act = whnfIO $ catch act err
        err = (\e@(MyException _) -> putStrLn $ "caught: " ++ show e)
    t <- measure w'act 1
    putStrLn $ "Total time: " ++ show (measTime $ fst t)
~~~~

We shall test with various recursion depths, on the two fuctions under discussion:

~~~~ {.haskell fancydiff=1 }
main :: IO ()
main = do
    putStrLn "------ With lots of catches"
    timeIt $ print $ runST        $ fibMod   50000000
    timeIt $ print $ runSTthrowIO $ fibMod_E 50000000
    putStrLn "\n------ With just one catch"
    timeIt $ print $ runST        $ fibMod   25000001
    timeIt $ print $ runSTthrowIO $ fibMod_E 25000001
    putStrLn "\n------ With no catch"
    timeIt $ print $ runST        $ fibMod   25000000
    timeIt $ print $ runSTthrowIO $ fibMod_E 25000000
~~~~

And the result is:

~~~~ {.shell fancydiff=1 mark1Start="{*\ " mark1End="\ *}" mark1Class=sourceMarker }
------ With lots of catches
caught: MyException (-5541175486947481557)
Total time: 0.6768422469031066
-1
Total time: 1.5523557260166854

------ With just one catch
caught: MyException 5934185968946882193
Total time: 0.32111207104753703
-1
Total time: 0.7558517289580777

------ With no catch
caught: MyException 4809429493926266912
Total time: 0.32045713800471276
caught: MyException 4809429493926266912
{* Total time: 0.26791215199045837 *}
~~~~

The first two results are of no surprise. Both the `if` and `catch` incur their overheads. The last result is more peculiar, because it suggests that the code for `fibMod_E` emanated from the compiler is _even faster_, despite of the `if`, as long as there are no wrapping `catch`'s in the evaluation. The difference probably boils down to the generated machine code, but I'd leave that to a topic of a different post.

### A few extra tests

(edit: added March 2, 2016)

Edward Kmett [pointed out](https://www.reddit.com/r/haskell/comments/47evoh/performance_of_the_st_monad_with_pure_exceptions/d0cj9z1) on Reddit that perhaps it would be interesting to test with `unsafeSTToIO`. So I've added the following cases (and also regenerated the results above, because every little change can affect the optimizer, and they varied slightly).

~~~~ {.haskell fancydiff=1 }
    putStrLn "------ With lots of catches"
    timeIt $ (unsafeSTToIO $ fibMod   50000000) >>= print
    putStrLn "\n------ With just one catch"
    timeIt $ (unsafeSTToIO $ fibMod   25000001) >>= print
    putStrLn "\n------ With no catch"
    timeIt $ (unsafeSTToIO $ fibMod   25000000) >>= print
~~~~

The additional output is:

~~~~ {.shell fancydiff=1 mark1Start="{*\ " mark1End="\ *}" mark1Class=sourceMarker }
------ With lots of catches
caught: MyException (-5541175486947481557)
Total time: 0.5766234899638221

------ With just one catch
caught: MyException 5934185968946882193
Total time: 0.2883113300194964

------ With no catch
caught: MyException 4809429493926266912
{* Total time: 0.29055844293907285 *}
~~~~

For the first two cases the result are around 11% better than the pure `runST`. Interestingly, for the third one `runSTthrowIO` still wins.

An even more drastic approach is to use `unsafeIOToST` and `unsafeSTToIO` in conjunction, modifying the original `fibMod`, allowing to freely insert the the less pure `IO`-based `catch` while keeping it in `ST` only from an API's perspective. It's not entirely sound in terms of exception handling, but it is worth presenting.

~~~~ {.haskell fancydiff=1 mark1Start="{*\ " mark1End="\ *}" mark1Class=sourceMarker  }
fibMod_H :: Int -> ST s Int
fibMod_H n =
    if n < 2
       then return n
       else do x <- newSTRef 0
               y <- newSTRef 1
               fibMod' n x y

    where
        fibMod' 0 x _ = readSTRef x
        fibMod' n' x y = do
            x' <- readSTRef x
            y' <- readSTRef y
            writeSTRef x y'
            writeSTRef y $! x'+y'
            when (n' == 1000) $ do
                throw $ MyException x'

            let recurse = fibMod' (n'-1) x y
            if n <= 25000000
               then recurse
               else {* unsafeIOToST $ *}
                       catch ({* unsafeSTToIO *} recurse)
				       (\(MyException _) -> return (-1))
~~~~

With the prints:

~~~~ {.haskell fancydiff=1 }
    putStrLn "------ With lots of catches"
    timeIt $ print $ runST        $ fibMod_H 50000000
    putStrLn "\n------ With just one catch"
    timeIt $ print $ runST        $ fibMod_H 25000001
    putStrLn "\n------ With no catch"
    timeIt $ print $ runST        $ fibMod_H 25000000
~~~~

However, it does not improve on our cases:

~~~~ {.shell fancydiff=1 mark1Start="{*\ " mark1End="\ *}" mark1Class=sourceMarker }
------ With lots of catches
-1
Total time: 3.401594614959322

------ With just one catch
-1
Total time: 1.2730798620032147

------ With no catch
caught: MyException 4809429493926266912
Total time: 0.30070900300052017
~~~~

The first two cases are considerably worse, and the third is still a bit better than using `STCatch`, but it's not representive of the common case where `catch` is probably going to appear in the evaluation at least once. Final conclusion is that pure exceptions are still a win, if we wish to remain sound.
