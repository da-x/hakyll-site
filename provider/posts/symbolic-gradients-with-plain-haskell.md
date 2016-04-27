---
excerpt: Haskell
title: Computing symbolic gradient vectors with plain Haskell
published: 2016-04-27
author: dan
---

While writing my [previous post](http://blog.aloni.org/posts/backprop-with-tensorflow/), I was curious how easy it would be to implement [TensorFlow](https://www.tensorflow.org)'s automatic differentiation for back propagation. In TensorFlow's web site they call it 'automatic differentiation' but in fact they probably do 'symbolic differentiation', as [mentioned in their white paper](http://download.tensorflow.org/paper/whitepaper2015.pdf). The difference between the two relates to whether the differentiation is done during the original computation or beforehand. It makes sense to do the latter, because then you can maintain a separate computational graph of the back propagation to perform the updates.

I've looked into this topic in the [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)) ecosystem, and found many useful and extensive libraries, namely [ad](https://hackage.haskell.org/package/ad) by Edward Kmett. However, to use these libraries or understand many of the blog posts on the subject requires some advanced Haskell, and I was wondering whether one can get going with differentiation using very basic and lean use of Haskell.

_So, how easy it would be to compute gradients of single-output functions, using Haskell with only the basic arsenal at our hands?_

# Data and imports

First, we perform some imports and declare the basic data type to hold our expression tree:

~~~~ {.haskell fancydiff=1 }
import           Control.Monad (forM_)
import qualified Data.Map      as Map

data Expr
   = Term Int       -- 'Term 0' is x0, 'Term 1' is x1, etc..
   | Lit Float      -- Constant numbers
   | Neg Expr       -- -f
   | Mul Expr Expr  -- a + b
   | Add Expr Expr  -- a * b
   | Sin Expr       -- sin a
   | Cos Expr       -- cos a
   deriving (Show, Eq, Ord)
~~~~
~

# Poor man's pretty-printing

One cannot go by without a nice String representation:

~~~~ {.haskell fancydiff=1 }
fshow :: Expr -> String
fshow (Term v)    = concat ["x", show v]
fshow (Lit v)     = show v
fshow (Mul e1 e2) = concat ["(", fshow e1, " * ", fshow e2, ")"]
fshow (Add e1 e2) = concat ["(", fshow e1, " + ", fshow e2, ")"]
fshow (Neg e)     = concat ["-(", fshow e, ")"]
fshow (Sin e)     = concat ["sin(", fshow e, ")"]
fshow (Cos e)     = concat ["cos(", fshow e, ")"]
~~~~

This implementation is basic that a sequence of summations will bear a horrible representation similar to `(x1 + (x2 + (x3 + (...))))` - however it's enough to get us going.

# Sample

The [Wikipedia page for Automatic Differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation) uses the following function:

$$
(x_1, x_2) = sin x_1 + x_1x_2
$$

Should be easy enough to represent it with our Haskell data, and use `fshow` from above:

~~~~ {.haskell fancydiff=1 }
λ> let wikipediaFunc = (Sin (Term 1)) `Add` ((Term 1) `Mul` (Term 2))

λ> fshow wikipediaFunc
"(sin(x1) + (x1 * x2))"
~~~~

# Gradient

The `gradient` function below takes an expression, and returns a map from each term number to the expression that computes it. The definition of the function is recursive and based on known simple derivation rules:

~~~~ {.haskell fancydiff=1 }
gradient :: Expr -> Map.Map Int Expr
gradient (Neg e)     = Map.map Neg (gradient e)
gradient (Cos e)     = Map.map (Mul (Neg (Sin e))) (gradient e)
gradient (Sin e)     = Map.map (Mul (Cos e)) (gradient e)
gradient (Term i)    = Map.fromList [(i, Lit 1.0)]
gradient (Lit _)     = Map.empty
gradient (Add e1 e2) = Map.unionWith Add (gradient e1) (gradient e2)
gradient (Mul e1 e2) = Map.unionWith Add (Map.map (Mul e2) (gradient e1))
                                         (Map.map (Mul e1) (gradient e2))
~~~~

The interesting parts are where `Map.unionWith` is used for addition and multiplication. Notice how easily the `Mul` part relates to the known derivation rule:

$$(f(x)g(x))' = g(x)f'(x) + g'(x)f(x)$$

The documentation for [Data.Map](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html) can tell about `Map.map` and `Map.unionWith`.

# Small helpers

Before testing it, we'll add just two helper functions. The first function simplifies expressions by getting rid of the $1.0$ literals we have added.

~~~~ {.haskell fancydiff=1 }
simplify :: Expr -> Expr
simplify (Mul (Lit 1.0) e) = simplify e
simplify (Mul e (Lit 1.0)) = simplify e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify (Mul e1 e2) = Mul (simplify e1) (simplify e2)
simplify e = e
~~~~

The second function will do all the work at the program's top level to compute the gradient and print it:

~~~~ {.haskell fancydiff=1 }
showGradient :: Expr -> IO ()
showGradient func = do
    putStrLn $ "f(..) = " ++ fshow func

    forM_ (Map.toList $ deriveMany func) $ \(k, v) -> do
        putStrLn $ "∂f / ∂" ++ fshow (Term k) ++ " = " ++ (fshow . simplify) v
~~~~

# Does it work?

~~~~ {.haskell fancydiff=1 }
> showGradient wikipediaFunc

f(..) = (sin(x1) + (x1 * x2))
∂f / ∂x1 = (cos(x1) + x2)
∂f / ∂x2 = x1
~~~~

Looks that it does. We have arrived at the same results as Wikipedia.

$$
\begin{align}
& \frac{∂ f}{∂ x_1} = cos x_1 + x_2 \\
& \frac{∂ f}{∂ x_2} = x_1 \\
\end{align}
$$

Will it work with something more complex?

~~~~ {.haskell fancydiff=1 }
> showGradient (Sin (Mul (Term 2 `Add` Lit 5.1) $ Cos (Term 1))) `Mul` (Term 1) `Mul` (Term 3)

f(..) = ((sin(((x2 + 5.1) * cos(x1))) * x1) * x3)
∂f / ∂x1 = (x3 * ((x1 * (cos(((x2 + 5.1) * cos(x1))) * ((x2 + 5.1) * -(sin(x1))))) + sin(((x2 + 5.1) * cos(x1)))))
∂f / ∂x2 = (x3 * (x1 * (cos(((x2 + 5.1) * cos(x1))) * cos(x1))))
∂f / ∂x3 = (sin(((x2 + 5.1) * cos(x1))) * x1)
~~~~

Comparing with [Wolfram Alpha](https://www.wolframalpha.com/input/?i=derive+((sin(((x2+%2B+5.1)+*+cos(x1)))+*+x1)+*+x3)), it seems to get it right.

# End note

Advance extensions of what I illusrated here can add a considerable amount of functionality and ease of use. We will definitely need to support matrices for instance, if we would like to derive a back-propagation graph. You can browse the [ad](https://hackage.haskell.org/package/ad) package to get some ideas.
