package com.study.fp.chapter4

import java.util.regex._

import scala.annotation.tailrec

/**
 * Created by Vineeth on 18/11/16.
 */
sealed trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map[Option[B]](f).getOrElse(None)

  def orElse[B >: A](other: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(other)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)
}

case object None extends Option[Nothing]

case class Some[A](get: A) extends Option[A]


object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def bothMatch1(pat: String, pat2: String, s: String): Option[Boolean] =
    map2[String => Boolean, String => Boolean, Boolean](mkMatcher(pat), mkMatcher(pat2))(_(s) && _(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
     a.foldRight[Option[List[A]]](Some(Nil))((e, acc) =>  map2(e, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((e, acc) =>  map2(f(e), acc)(_ :: _))
  }
}
