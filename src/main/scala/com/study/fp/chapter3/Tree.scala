package com.study.fp.chapter3


/**
 * Created by Vineeth on 18/11/16.
 */

sealed trait Tree[+ A] {

  // Exercise 3.25
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  // Exercise 3.27
  def depth: Int = this match {
    case Leaf(_)      => 1
    case Branch(l, r) => (l.depth max r.depth) + 1
  }

  // Exercise 3.28
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(e)      => Leaf(f(e))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  // Exercise 3.29
  def fold[B](f: A => B, g: (B, B) => B): B = this match {
    case Leaf(e)      => f(e)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
  }

  def size1: Int = fold[Int](_ => 1, (l, r) => l + r + 1)

  def depth1: Int = fold[Int](_ => 1, (l, r) => (l max r) + 1)

  def map1[B](f: A => B): Tree[B] = fold[Tree[B]](e => Leaf(f(e)), (l, r) => Branch(l, r))
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.26
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(e)      => e
    case Branch(l, r) => max(l) max max(r)
  }
}
