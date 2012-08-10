package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(l)             => l
      case Branch(left, right) => math.max(maximum(left), maximum(right))
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_)             => 0
      case Branch(left, right) => math.max(1 + depth(left), 1 + depth(right))
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(a)             => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A], f0: Leaf[A] => B)(f: (Tree[A], B) => B): B =
    tree match {
      case Leaf(a)             => f0(Leaf(a))
      case Branch(left, right) => f(left, fold(right, f0)(f))
    }

  def fold1[A, B](tree: Tree[A], f0: A => B)(f: (B, B) => B): B =
    tree match {
      case Leaf(a)             => f0(a)
      case Branch(left, right) => f(fold1(left, f0)(f), fold1(right, f0)(f))
    }

  def size2[A](tree: Tree[A]): Int =
    fold(tree, (l: Leaf[A]) => 1)((_, res) => res + 1)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree, (_:Leaf[Int]).value)((cur, res) => math.max(maximum2(cur), res))

  def depth2[A](tree: Tree[A]): Int =
    fold(tree, (l: Leaf[A]) => 0)((cur, res) => math.max(1 + depth2(cur), 1 + res))

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree, (l: Leaf[A]) => Leaf(f(l.value)): Tree[B])((cur, res) => Branch(map2(cur)(f), res))


  def size3[A](tree: Tree[A]): Int =
    fold1(tree, (a: A) => 1)(_ + _)

  def maximum3(tree: Tree[Int]): Int =
    fold1(tree, (n: Int) => n)((l, r) => math.max(l, r))

  def depth3[A](tree: Tree[A]): Int =
    fold1(tree, (a: A) => 0)((l, r) => math.max(1 + l, 1 + r))

  def map3[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold1(tree, (a: A) => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
}