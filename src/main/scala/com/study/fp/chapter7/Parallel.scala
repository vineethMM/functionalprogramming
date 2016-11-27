package com.study.fp.chapter7

import scala.concurrent.duration.TimeUnit

/**
 * Created by Vineeth on 23/11/16.
 */

// Data Type to represent an asynchronous computation.
//
//* Par[A]


// for taking an unevaluated A and returning a computation that might evaluate it in
// a separate thread. We call it unit because in a sense it creates a unit of parallelism
// that just wraps a single value
//
//* def unit[A](a: => A): Par[A]

// for extracting the resulting value from a parallel computation.
//
//* def get[A](a: Par[A]): A



// Exercise  7.1
// Par.map2 is a new higher-order function for combining the result of two parallel computations.
// What is its signature? Give the most general signature possible
//
// def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => B): Par[B]


// Exercise 7.2
// Before continuing, try to come up with representations for Par that make it possible to
// implement the functions of our API.
//
// Par[A] should represent a function which takes in a mechanism to run a parallel computation
// and yields value of type `A`
//
// so type Par[A] = EM => A
// where `EM` represents a Execution Mechanism.


trait ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] =
    es => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  // Exercise 7.3
  // Fix the implementation of map2 so that it respects the contract of timeouts on Future.
  def map2TimeOut[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      new Future[C] {
        def get = f(af.get, bf.get)

        def get(timeout: Long, unit: TimeUnit): C  = {
          val startTime = System.nanoTime()
          val aVal = af.get(timeout, unit)
          val endTime = System.nanoTime()

          f(aVal, bf.get(timeout - (endTime - startTime), unit))
        }

        def isDone = af.isDone && bf.isDone

        def isCancelled = af.isCancelled || bf.isCancelled

        def cancel(evenIfRunning: Boolean): Boolean = false
      }
    }

  // Exercise 7.4
  // using lazyUnit, write a function to convert any function A => B to one that evaluates
  // its result asynchronously.
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))


  def parMapPrimitve[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    es => UnitFuture(ps.map(asyncF(f)).map(a => a(es).get))

  // Exercise 7.5
  // Write this function, called sequence. No additional primitives are required. Do not call run.
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))(map2(_, _)(_ :: _))

  // we’ve wrapped our implementation in a call to fork. With this implementation, parMap will
  // return immediately, even for a huge input list. When we later call run, it will fork a single
  // asynchronous computation which itself spawns N parallel computations, and then waits for
  // these computations to finish, collecting their results into a list.
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))

    sequence(fbs)
  }

  // Exercise 7.6
  // Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =  fork{
    val fbs: List[Par[(Boolean, A)]] = as.map(a => lazyUnit(f(a), a))

    fbs.foldRight[Par[List[A]]](unit(Nil)){
      map2(_, _){
        case ((true, a), acc)  => a :: acc
        case ((false, a), acc) => acc
      }
    }
  }

}
