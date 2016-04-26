---
excerpt: ML
title: Back propagation with TensorFlow
published: 2016-04-26
author: dan
---

When I first read about neural network in Michael Nielsen's [Neural Networks and Deep Learning](http://neuralnetworksanddeeplearning.com/), I was excited to find a good source that explains the material along with actual code. However there was a rather steep jump in the part that describes the basic math and the part that goes about implementing it, and it was especially apparant in the  `numpy`-based code that implements backward propagation.

So, in order to explain it better to myself, and learn about [TensorFlow](https://www.tensorflow.org/) in the process, I took it upon myself to implement the first network in the book using TensorFlow by two means. First, manually defining the back propagation step, and the second - letting TensorFlow do the hard work.

# Setup

First, we need to load TensorFlow and setup the basic parts of the graph - inputs (`a_0`, `y`), and states (`w_1`, `b_1`, `w_2`, `b_2`).

``` { .python fancydiff=on }
#!/usr/bin/env python

import tensorflow
import tensorflow as tf
import sys

from tensorflow.examples.tutorials.mnist import input_data
mnist = input_data.read_data_sets("MNIST_data/", one_hot=True)

a_0 = tf.placeholder(tf.float32, [None, 784])
y = tf.placeholder(tf.float32, [None, 10])

middle = 30
w_1 = tf.Variable(tf.truncated_normal([784, middle]))
b_1 = tf.Variable(tf.truncated_normal([1, middle]))
w_2 = tf.Variable(tf.truncated_normal([middle, 10]))
b_2 = tf.Variable(tf.truncated_normal([1, 10]))
```

## The sigmoid function

Our sigmoid function, although provided by TensorFlow's extensive function library, is brought here as reference:

$$
\sigma(z) = \frac{1}{1+e^{-z}}
$$

``` { .python fancydiff=on }
def sigma(x):
    return tf.div(tf.constant(1.0),
                  tf.add(tf.constant(1.0), tf.exp(tf.neg(x))))
```

## The forward propagation

Provided that the input image is given by the $ a_0 $ matrix, calculating forward propagation for multiple
images at a time can be done with simple matrix multiplication, defined as such:

$$
\begin{align}
& z_1 = a_0 \cdot w_1 + b_1 \\
& a_1 = \sigma(z_1) \\
& z_2 = a_1 \cdot w_2 + b_2 \\
& a_2 = \sigma(z_2) \\
\end{align}
$$

Given a tensor of _multiple images_, this can done in TensorFlow for _all_ them at the same time (thanks to 'broadcasting'),
so the above gets a one-to-one transltion in TensorFlow:


``` { .python fancydiff=on }
z_1 = tf.add(tf.matmul(a_0, w_1), b_1)
a_1 = sigma(z_1)
z_2 = tf.add(tf.matmul(a_1, w_2), b_2)
a_2 = sigma(z_2)
```

## Difference

The input provides $y$ as the test for the accuracy of the network's output, so we compute the following vector:

$$
\begin{align}
& \nabla a = a_2 - y \\
\end{align}
$$


``` { .python fancydiff=on }
d_a = tf.sub(a_2, y)
```

## The sigmoid prime function

Here's the derivate of the sigmoid function from above, which will be needed during the backward propagation:

$$
\sigma'(z) = \sigma(z)(1 - \sigma(z))
$$

``` { .python fancydiff=on }
def sigmaprime(x):
    return tf.mul(sigma(x), tf.sub(tf.constant(1.0), sigma(x)))
```

## Backward propagation {#sec:back}

The most complicated part is the backward propagation. First, we need to compute the deltas of the weights and biases. In the original book the Python code was a bit puzzling, but here we can describe the same algorithm in a functional, stateless way.

$$
\begin{align}
& \nabla z_2 = \nabla a \cdot \sigma'(z_2) \\
& \nabla b_2 = \nabla z_2 \\
& \nabla w_2 = a_1^T \cdot \nabla z_2 \\
& \\
& \nabla a_1 = \nabla z_2 \cdot w_2^T \\
& \nabla z_1 = \nabla a_1 \cdot \sigma'(z_1) \\
& \nabla b_1 = \nabla z_1 \\
& \nabla w_1 = a_0^T \cdot \nabla z_1 \\
\end{align}
$$

It's also one-to-one with:

``` { .python fancydiff=on }
d_z_2 = tf.mul(d_a, sigmaprime(z_2))
d_b_2 = d_z_2
d_w_2 = tf.matmul(tf.transpose(a_1), d_z_2)

d_a_1 = tf.matmul(d_z_2, tf.transpose(w_2))
d_z_1 = tf.mul(d_a_1, sigmaprime(z_1))
d_b_1 = d_z_1
d_w_1 = tf.matmul(tf.transpose(a_0), d_z_1)
```

## Updating the network {#sec:upd}

We take the computed $\nabla$s and update the weights in one step. Note that the following does not precisely match the book - I have omitted the constant $1/n$ divider. For simplicity, it's not really needed, as it comes into play inside the $\eta$ itself, in this case.

$$
\begin{align}
& w_1 \leftarrow w_1 - \eta \cdot \nabla w_1 \\
& b_1 \leftarrow b_1 - \eta \cdot \nabla b_1 \\
& w_2 \leftarrow w_2 - \eta \cdot \nabla w_2 \\
& b_2 \leftarrow b_2 - \eta \cdot \nabla b_2 \\
\end{align}
$$


In TensorFlow, it can translate to a list of a assignments:

``` { .python fancydiff=on }
eta = tf.constant(0.5)
step = [
    tf.assign(w_1,
            tf.sub(w_1, tf.mul(eta, d_w_1)))
  , tf.assign(b_1,
            tf.sub(b_1, tf.mul(eta,
                               tf.reduce_mean(d_b_1, reduction_indices=[0]))))
  , tf.assign(w_2,
            tf.sub(w_2, tf.mul(eta, d_w_2)))
  , tf.assign(b_2,
            tf.sub(b_2, tf.mul(eta,
                               tf.reduce_mean(d_b_2, reduction_indices=[0]))))
]
```

## Running and testing the training process

The following will be able to train the network and test it in the meanwhile, using mini-batches of 10. Here, I chose to test with 1000 items from the test set, every 1000 mini-batches.

``` { .python fancydiff=on }
acct_mat = tf.equal(tf.argmax(a_2, 1), tf.argmax(y, 1))
acct_res = tf.reduce_sum(tf.cast(acct_mat, tf.float32))

sess = tf.InteractiveSession()
sess.run(tf.initialize_all_variables())

for i in xrange(10000):
    batch_xs, batch_ys = mnist.train.next_batch(10)
    sess.run(step, feed_dict = {a_0: batch_xs,
                                y : batch_ys})
    if i % 1000 == 0:
        res = sess.run(acct_res, feed_dict =
                       {a_0: mnist.test.images[:1000],
                        y : mnist.test.labels[:1000]})
        print res
```

Running it shows that it manages to train the network, as we quickly get 923 correct out of 1000 tests.

``` { .shell fancydiff=on }
Extracting MNIST_data/train-images-idx3-ubyte.gz
Extracting MNIST_data/train-labels-idx1-ubyte.gz
Extracting MNIST_data/t10k-images-idx3-ubyte.gz
Extracting MNIST_data/t10k-labels-idx1-ubyte.gz
93.0
741.0
870.0
909.0
904.0
912.0
916.0
923.0
```

## Automatic deriviation

The great part about TensorFlow is its ability to derive the step function on its own. So, instead of the rather complicated 'Backward propagation' and 'Updating the network' given above for educational purposes ([@sec:back] and [@sec:upd]), we can simply write:


``` { .python fancydiff=on title="Step function alternative" }
cost = tf.mul(diff, diff)
step = tf.train.GradientDescentOptimizer(0.1).minimize(cost)
```
And observe that the training still works.
