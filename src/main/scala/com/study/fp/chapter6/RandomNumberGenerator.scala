package com.study.fp.chapter6

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

  val maxValue = Int.MaxValue.toDouble + 1

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng1)  = rng.nextInt

    if(int != Int.MinValue) (math.abs(int), rng1) else nonNegativeInt(rng1)
  }

  // Exercise 6.2

  def double(rng: RNG): (Double, RNG) = {
    val (int, rng1) = nonNegativeInt(rng)

    (int / maxValue, rng1)
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


  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
   rng => {
     val (a, rng1) = s(rng)

     (f(a), rng1)
   }



  // Exercise 6.5
  def double1: Rand[Double] =
    map[Int, Double](rng => rng.nextInt)(_ / maxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
   rng => {
     val (a, rng1) = ra(rng)
     val (b, rng2) = rb(rng1)

     (f(a, b), rng2)
   }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil))(map2(_, _)(_ :: _))

  def ints1(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill[Rand[Int]](count)(rn => rn.nextInt))(rng)

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
   rng => {
     val (a, rng1) = f(rng)

     g(a)(rng1)
   }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap[Int, Int](nonNegativeInt)( a => {
      val mod = a % n
      if (a + (n-1) - mod >= 0)
        unit(a)
      else
        nonNegativeLessThan(n)
    })

  // Exercise 6.9
  def mapNew[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2New[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)( a => flatMap(rb)(b => unit(f(a, b))))

}