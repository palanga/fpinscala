package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeWithFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maxWithFold(tree: Tree[Int]): Int = fold(tree)(v => v)(_ max _)

  def depthWithFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(1 + _ max _)

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

}