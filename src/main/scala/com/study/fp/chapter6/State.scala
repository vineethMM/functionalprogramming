package com.study.fp.chapter6

/**
 * Created by Vineeth on 22/11/16.
 */

// Exercise 6.10
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State( run(_) match { case (a, s) => (f(a), s)} )

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s1) = this.run(s)
      val (b, s2) = sb.run(s1)

      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => run(s) match { case (a, s1) => f(a).run(s1) })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object State {

  def unit[A, S](a: A): State[S, A] = State[S, A]((a, _))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight[State[S, List[A]]](unit(Nil))(_.map2(_)(_ :: _))
}