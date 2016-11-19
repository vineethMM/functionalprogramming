package com.study.fp.chapter4

/**
 * Created by Vineeth on 19/11/16.
 */
sealed trait EitherSeq[+E, +A] {
  def map[B](f: A => B): EitherSeq[E, B] = this match {
    case RightSeq(a)    => RightSeq(f(a))
    case LeftSeq(e)     => LeftSeq(e)
  }

  def flatMap[EE >: E, B](f: A => EitherSeq[EE, B]): EitherSeq[EE, B] = this match {
    case RightSeq(a) => f(a)
    case LeftSeq(e)  => LeftSeq(e)
  }

  def orElse[EE >: E,B >: A](b: => EitherSeq[EE, B]): EitherSeq[EE, B] = this match {
    case RightSeq(_) => this
    case LeftSeq(_)  => b
  }

  def map2[EE >: E, B, C](b: EitherSeq[EE, B])(f: (A, B) => C):EitherSeq[EE, C] = (this, b) match {
    case (RightSeq(a), RightSeq(b))          => RightSeq(f(a, b))
    case (LeftSeq(errs1), LeftSeq(errs2))    => LeftSeq(errs1 ++ errs2)
    case (LeftSeq(e1), _)                    => LeftSeq(e1)
    case (_, LeftSeq(e1))                    => LeftSeq(e1)
  }
}

case class LeftSeq[+E](value: Seq[E]) extends EitherSeq[E, Nothing]

case class RightSeq[+A](value: A) extends EitherSeq[Nothing, A]

object EitherSeq {
  // Exercise 4.7
  def sequence[E, A](es: List[EitherSeq[E, A]]): EitherSeq[E, List[A]] =
    es.foldRight[EitherSeq[E, List[A]]](RightSeq(Nil))(_.map2(_)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => EitherSeq[E, B]): EitherSeq[E, List[B]] =
    as.foldRight[EitherSeq[E, List[B]]](RightSeq(Nil))(f(_).map2(_)(_ :: _))
}
