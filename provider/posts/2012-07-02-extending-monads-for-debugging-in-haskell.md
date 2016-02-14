---
title: Extending Monads for debugging in Haskell
link: http://www.aloni.org/extending-monads-for-debugging-in-haskell/
author: dan
post_id: 331
created: 2012/07/02 13:39:25
created_gmt: 2012/07/02 10:39:25
comment_status: open
post_name: extending-monads-for-debugging-in-haskell
status: publish
post_type: post
---

One of the nice things about [Haskell](http://www.haskell.org/haskellwiki/Haskell) is the ability to extend the class of [Monads](http://www.haskell.org/tutorial/monads.html).

One of the original purposes of Monads was to describe flow while leaving the implementation of the flow to a later stage. This allows to define what happens as a side effect of the computational steps.

For example, let's say we have a computation that we would like to debug. If we formulate it algebraically, it would be harder to checks the step of the computation in run-time. So naturally we break the computation into statement-like stages, introducing code to trace the intermediate results in between.

However, sometimes we would also like to **keep the performance of the 'untraced' computation as it was**. In C++ we can can use templates in order to instantiate two implemtations of the computation. In C we would probably use macros trickery of some sort along with static inline functions that define to nothing. In Python we would probably use a global debug variable, the `__debug__` builtin, or a dedicated logging library.

In Haskell, this can come naturally as an extension of the Monad class, with the advantage of multiple instantation. To illustrate, let's extend Monad with MonadDebug:

```haskell
import Control.Monad.Identity (runIdentity, Identity)

class Monad m => MonadDebug m where
  logDebug :: String -> m ()

instance MonadDebug IO where
  logDebug s = putStrLn s

instance MonadDebug Identity where
  logDebug _ = return ()
```

Those instances make it possible to use class function `logDebug` directly under IO or under pure computations with the Identity Monad.

Let's define a sample computation function:


```haskell
computation :: MonadDebug m => Integer -> Integer -> m Integer
computation x y = do
  let t1 = x * 2 + y
  logDebug $ "Here, t1=" ++ (show t1)
  let t2 = t1 * 3 + x
  logDebug $ "Here, t2=" ++ (show t2)
  let t3 = t2 * 7 - x * x
  logDebug $ "Here, t3=" ++ (show t3)
  return t3
```

The type signature for `computation` is optional, and can be inferred by the compiler, simply because we referenced logDebug under our Monad.

Now, let's try to use it under the two environments. Here's the code:


```haskell
main :: IO ()
main = do
  putStrLn "Run computation as pure:"
  let t = runIdentity $ computation 1 2
  putStr "Result: "
  print t
  putStrLn ""

  putStrLn "Run computation with impure IO logging stages: "
  t' <- computation 1 2
  putStr "Result: "
  print t'
```

Let's try to run it:


```haskell
# runghc test.hs
Run computation as pure:
Result: 90

Run computation with impure IO logging stages:
Here, t1=4
Here, t2=13
Here, t3=90
Result: 90
```

The advantage is that the compiler can optimize the pure Identity Monad computation much better compared to the non-pure computation, and we achieve this without using any Haskell constructs that are much sophisticated.

p.s. A novice reader might also be able to devise MonadDebug instances for the various Monad transformer classes under the cases where MonadIO is the underlying Monad.