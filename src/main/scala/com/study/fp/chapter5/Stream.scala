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

  // Exercise 5.13
  def zipWith[B](o: Stream[B]): Stream[(A, B)] =
    unfold[(A, B), (Stream[A], Stream[B])]((this, o)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def zipAll[B](o: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, o)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty)        => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2))        => Some((None, Some(h2())), (Empty, t2()))
      case (Empty, Empty)               => None
    }

  // Exercise 5.14
  def startsWith[B](o: Stream[B]): Boolean = (this, o) match {
    case (Cons(h1, t1), Cons(h2, t2)) => h1() == h2() && t1().startsWith(t2())
    case (Empty, _)                   => false
    case (_, Empty)                   => true
  }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold[Stream[A], Stream[A]](this){
      case Empty      => None
      case Cons(h, t) => Some((Cons(h, t), t()))
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight[Stream[B]](cons(z, Empty)){
      case (e, Cons(h, t)) => cons(f(e, h()), Cons(h, t))
    }
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

  // Exercise 5.13
  def map[A, B](s: Stream[A])(f: A => B) = unfold[B, Stream[A]](s){
    case Cons(a, b) => Some((f(a()), b()))
    case Empty      => None
  }

  def take[A](s: Stream[A], n: Int) = unfold[A, (Int, Stream[A])]((n, s)){
    case (c, Cons(a, b)) if c > 0 => Some(a(),(c -1, b()))
    case _                        => None
  }

  def takeWhile[A](s: Stream[A])(p: A => Boolean) = unfold[A, Stream[A]](s) {
    case Cons(h, t) if(p(h())) => Some((h(), t()))
    case _                     => None
  }
}
