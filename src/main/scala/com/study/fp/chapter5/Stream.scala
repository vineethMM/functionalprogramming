package com.study.fp.chapter5

/**
 * Created by Vineeth on 19/11/16.
 */
sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _                   => Empty
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(acc: Stream[A], inner: Stream[A]): Stream[A] = inner match {
      case Cons(h, t) if p(h()) => loop(cons(h(), acc), t())
      case _                    => acc
    }

    loop(Empty, this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)(p(_) || _)

  // Exercise 5.4
  def forAll(f: A => Boolean): Boolean = foldRight(true)(f(_) && _)

  // Exercise 5.5
  def takeWhile1(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if(f(a)) cons(a, b) else Empty)

  // Exercise 5.6
  def headOption1: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => cons(f(a), b))

  def filter(f : A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if(f(a)) cons(a, b) else b)

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons[Int](n, from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def loop(nMinus1: Int, n: Int): Stream[Int] =
      cons(nMinus1, loop(n, n + nMinus1))

    loop(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None         => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // Exercise 5.12
  def fibs1: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)){ case (a, b) => Some((a, (b, a + b)))}

  def from1(n: Int): Stream[Int] = unfold[Int, Int](n)(s => Some((s, s + 1)))

  def constant(n: Int): Stream[Int] = unfold[Int, Int](n)(s => Some((s, s)))

  def ones: Stream[Int] = unfold[Int, Int](1)(s => Some((s, s)))
}
