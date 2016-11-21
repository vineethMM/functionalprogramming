package com.study.fp.chpater5

/**
 * Created by Vineeth on 21/11/16.
 */

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG (newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

object RNG {

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng1)  = rng.nextInt

    if(int != Int.MinValue) (math.abs(int), rng1) else nonNegativeInt(rng1)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (int, rng1) = nonNegativeInt(rng)

    if(int != Int.MaxValue) (int.toDouble/Int.MaxValue, rng1) else double(rng1)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)

    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt

    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    List.range(0, count).foldLeft[(List[Int], RNG)]((Nil, rng)){
      case ((acc, s), _) => {
        val (int, nextRng) = s.nextInt

        (int :: acc, nextRng)
      }
    }
  }


  type RAND[+A] = RNG => (A, RNG)

  def unit[A](a: A): RAND[A] = rng => (a, rng)

  def map[A, B](s: RAND[A])(f: A => B): RAND[B] =
   rng => {
     val (a, rng1) = s(rng)

     (f(a), rng1)
   }

  // Exercise 6.5
  def double1: RAND[Double] =
    map[Int, Double](rng => rng.nextInt)( i => i.toDouble/Int.MaxValue)

}