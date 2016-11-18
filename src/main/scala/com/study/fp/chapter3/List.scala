package com.study.fp.chapter3

import scala.annotation.tailrec
import scala.{List => ScalaList}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](h: A, tail: List[A]) extends List[A]

object List {

  def apply[A](xs: A*): List[A] =
    if(xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail:_*))

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
  // Using foldRight to preserve ordering of elements
  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight[A, List[A]](xs, ys)(Cons(_, _))

  // Exercise 3.15
  // Using foldRight to preserve ordering of elements
  def flatten[A](xs: List[List[A]]): List[A] =  foldRight[List[A], List[A]](xs, Nil)(append)

  // Exercise 3.16
  def addOne(xs: List[Int]): List[Int] = foldRight[Int, List[Int]](xs, Nil)((x, acc) => Cons(x + 1, acc))

  // Exercise 3.17
  def doubleToString(xs: List[Double]): List[String] =
    foldRight[Double, List[String]](xs, Nil)( (x, acc) => Cons(x.toString, acc))

  // Exercise 3.18
  def map[A, B](xs:List[A])(f : A => B): List[B] = foldRight[A, List[B]](xs, Nil)( (x, acc) => Cons(f(x), acc))

  // Exercise 3.19
  // use this function to remove odd numbers from a list of integers
  // filter(List(1,2,3,4), _ % 2 == 0)
  def filter[A](xs: List[A], predicate: A => Boolean): List[A] = foldRight[A, List[A]](xs, Nil){
    case (x, acc) if predicate(x) => Cons(x, acc)
    case (_, acc)                 => acc
  }

  // Exercise 3.20
  def flatMap[A, B](xs: List[A])(f: A=> List[B]): List[B] =
    foldRight[A, List[B]](xs, Nil)((x, acc) => append(f(x), acc))

  // Exercise 3.21
  def filterUsingFlatMap[A](xs: List[A], predicate: A => Boolean) =
    flatMap(xs)(a => if(predicate(a)) Cons(a, Nil) else Nil)

  // Exercise 3.22
  def addElements(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Cons(x, xss), Cons(y, yss)) => Cons(x + y, addElements(xss, yss))
    case _                            => Nil
  }

  // Exercise 3.23
  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Cons(x, xss), Cons(y, yss)) => Cons(f(x,y), zipWith(xss, yss)(f))
    case _                            => Nil
  }

  def take[A](xs: List[A],n: Int): List[A] = {
    def loop(acc: List[A], xss: List[A],count: Int): List[A] = xss match {
      case Cons(h, t) if count > 0 => loop(Cons(h, acc), t, n-1)
      case _                       => acc
    }

    loop(Nil, xs, n)
  }

  def takeWhile[A](xs: List[A],f: A => Boolean): List[A] = {
    def loop(acc: List[A], xss: List[A]): List[A] = xss match {
      case Cons(h, t) if(f(h)) => loop(Cons(h, acc), t)
      case _                   => acc
    }

    loop(Nil, xs)
  }

  @tailrec
  def forAll[A](xs: List[A], f: A => Boolean): Boolean = xs match {
    case Nil        => true
    case Cons(h, t) => f(h) && forAll(xs, f)
  }

  @tailrec
  def exists[A](xs: List[A], f: A => Boolean): Boolean = xs match {
    case Nil        => false
    case Cons(h, t) => f(h) || exists(t, f)
  }

  def scanLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): List[B] =
    foldLeft[A, List[B]](xs, Cons(z, Nil)){ case (x, Cons(h, t)) => Cons(f(x, h), Cons(h, t)) }

  @tailrec
  def startsWith[A](xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
    case (Cons(x, xss), Cons(y, yss)) => (x == y) && startsWith(xss, yss)
    case _                            => true
  }

  // Exercise 3.24
  @tailrec
  def hasSubSequence[A](xs: List[A], ys: List[A]): Boolean =
    startsWith(xs, ys) || (xs match {
      case Cons(h, t) if startsWith(t, ys) => true
      case Cons(h, t)                      => hasSubSequence(t, ys)
      case Nil                             => false
    })
}