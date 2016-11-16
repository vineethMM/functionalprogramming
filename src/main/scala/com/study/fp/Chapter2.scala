package com.study.fp

object Chapter2 {
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // Exercise 2.1
  // Write a function to get the nth Fibonacci number. The first two Fibonacci
  // numbers are 0 and 1, and the next number is always the sum of the previous two.
  // Your definition should use a local tail-recursive function
  def fib(n: Int): Int = {
    // local tail recursive function
    def loop(nMinusOne: Int, n: Int, count: Int): Int = count match {
      case 1 => nMinusOne
      case _ => loop(n, n + nMinusOne, count - 1)
    }

    loop(0,1, n)
  }

  // Exercise 2.2
  // Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function.
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def loop(index: Int, previousResult: Boolean): Boolean =
     if(index >= as.length) previousResult
     else loop(index + 1, gt(as(index -1), as(index)))

     loop(1, true)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => f(a, _)

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
