package com.study.fp.chapter4

/**
 * Created by Vineeth on 19/11/16.
 */

// Exercise 4.6
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a)    => Right(f(a))
    case Left(e)     => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e)  => Left(e)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case Left(_)  => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] =  for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
     es.foldRight[Either[E, List[A]]](Right(Nil))(_.map2(_)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))(f(_).map2(_)(_ :: _))
}




