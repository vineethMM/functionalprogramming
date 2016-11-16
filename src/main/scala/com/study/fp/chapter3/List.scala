package com.study.fp.chapter3

import scala.{List => ScalaList}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](h: A, tail: List[A]) extends List[A]

object List {
  // Exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(h, t) => t
    case Nil        =>  throw new NoSuchElementException("Nil.tail")
  }

  // Exercise 3.3
  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Cons(h, t) => Cons(newHead, t)
    case Nil        => Cons(newHead, Nil)
  }

  // Exercise 3.4
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Cons(h, t) if n > 0 => drop(t, n - 1)
    case _                   => Nil
  }

  // Exercise 3.5
  def dropWhile[A](xs: List[A], p: A => Boolean): List[A] =  xs match {
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _                  => Nil
  }

  // Exercise 3.6
  def init[A](xs: List[A]): List[A] = xs match {
    case Cons(e1, Cons(e2, t)) => Cons(e1, init(Cons(e2, t)))
    case _                     => Nil
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil        => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil        => 1.0
    case Cons(h, t) => h * product(t)
  }

  def foldRight[A, B](xs: List[A],z: B)(f: (A, B) => B): B = xs match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ints: List[Int]) = foldRight[Int, Int](ints, 0)(_ + _)

  def product2(ds: List[Double]) = foldRight[Double, Double](ds, 1.0)(_ * _)

  // Exercise 3.7
  // product2 implemented in terms for higher order function `foldRight` can't terminate
  // immediately when it encounters a `0.0` because the terminating condition for folRight
  // function is a `Nil` match which will be reached only when the given list is exhausted.

  // Exercise 3.8
  // When you pass the Data constructors to `foldRight` like this
  // foldRight(List(1,2,3), Nil: List[Int], Cons(_, _))
  // it will return the same list.
  // From this we can infer that foldLeft along with the data constructors can
  // be used to create list transformation functions.

  // Exercise 3.9
  def length[A](xs: List[A]): Int = foldRight[A, Int](xs, 0)((_, l) => l + 1)

  // Exercise 3.10
  def foldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
    def loop(acc: B, innerXs: List[A]): B = innerXs match {
      case Nil        => acc
      case Cons(h, t) => loop(f(h, acc), t)
    }

    loop(z, xs)
  }

  // Exercise 3.11
  def sum3(ints: List[Int]) = foldLeft[Int, Int](ints, 0)(_ + _)
  def product3(ds: List[Double]) = foldLeft[Double, Double](ds, 1.0)(_ * _)

  // Exercise 3.12
  def reverse[A](xs: List[A]) = foldLeft[A, List[A]](xs, Nil)(Cons(_, _))

  // Exercise 3.13
  def foldLeftWithFoldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldRight[A, B](reverse(xs), z)(f)

  def foldRightWithFoldLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B](reverse(xs), z)(f)

  // Exercise 3.14
  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight[A, List[A]](xs, ys)(Cons(_, _))

  // Exercise 3.15
  def flatten[A](xs: List[List[A]]): List[A] =  foldRight[List[A], List[A]](xs, Nil)(append)
}