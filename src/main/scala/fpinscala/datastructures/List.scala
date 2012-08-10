package fpinscala.datastructures

import fpinscala.structuringprograms.Exercises._
import annotation.tailrec

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val matchingExample = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    list match {
      case Nil         => z
      case Cons(a, as) => f(a, foldRight(as, z)(f))
    }

  def sum2(ints: List[Int]) =
    foldRight(ints, 0.0)(_ + _)
  
  def product2(ints: List[Double]) = 
    foldRight(ints, 1.0)(_ * _)

  def tail[A](list: List[A]): List[A] =
    drop(list)(1)

  def drop[A](list: List[A])(n: Int): List[A] =
    if (n <= 0) list
    else (list: @unchecked) match {
      case Cons(_, as) => drop(as)(n - 1)
    }


  @tailrec
  def dropWhile[A](list: List[A])(p: Pred[A]): List[A] =
    list match {
      case Cons(a, as) if (p(a)) => dropWhile(as)(p)
      case _                     => list
    }

  def setHead[A](list: List[A])(a: A): List[A] =
    list match {
      case Cons(_, as) => Cons(a, as)
      case _           => list
    }

  def init[A](list: List[A]): List[A] =
    (list: @unchecked)  match {
      case Cons(a, Nil) => Nil
      case Cons(a, as)  => Cons(a, init(as))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, result) => result + 1)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((result, _) => result + 1)

  @tailrec
  def foldLeft[A,B](list: List[A], z: B)(f: (B, A) => B): B =
    list match {
      case Nil         => z
      case Cons(a, as) => foldLeft(as, f(z, a))(f)
    }

  def foldRight2[A,B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(list, z)((res, cur) => f(cur, res))

  def foldLeft2[A,B](list: List[A], z: B)(f: (B, A) => B): B =
    foldRight(list, z)((cur, res) => f(res, cur))

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((res, cur) => Cons(cur, res))

  def append[A](list: List[A], a: A): List[A] =
    foldRight(list, List(a))((cur, res) => Cons(cur, res))

  def concatenate[A](lists: List[List[A]]): List[A] =
    foldRight(lists, Nil: List[A])((cur, res) => foldRight(cur, res)((c, r) => Cons(c, r)))

  def plusOne(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])((cur, res) => Cons(cur + 1, res))

  def asString(list: List[Double]): List[String] =
    foldRight(list, Nil: List[String])((cur, res) => Cons(cur.toString, res))

  def map[A,B](list: List[A])(f: A => B): List[B] =
    foldRight(list, Nil: List[B])((cur, res) => Cons(f(cur), res))

  def filter[A,B](list: List[A])(f: A => Boolean): List[A] =
    foldRight(list, Nil: List[A])((cur, res) => if (f(cur)) Cons(cur, res) else res)

  def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] =
    foldRight(list, Nil: List[B])((cur, res) => append(f(cur), res))

  def filter2[A,B](list: List[A])(f: A => Boolean): List[A] =
    flatMap(list)(a => if (f(a)) List(a) else List())

  def zipAdd(list1: List[Int], list2: List[Int]): List[Int] =
    list1 match {
      case Nil           => Nil
      case Cons(a1, as1) => list2 match {
        case Nil           => Nil
        case Cons(a2, as2) => Cons(a1 + a2, zipAdd(as1, as2))
      }
    }

  def zip[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] =
    list1 match {
      case Nil           => Nil
      case Cons(a1, as1) => list2 match {
        case Nil           => Nil
        case Cons(a2, as2) => Cons(f(a1, a2), zip(as1, as2)(f))
      }
    }

  def forall(list: List[Boolean]): Boolean =
    foldLeft(list, true)((res, cur) => res && cur)

  def startsWith[A](list: List[A], sub: List[A]): Boolean =
    forall(zip(list, sub)(_ == _))

  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean =
    list match {
      case Nil         =>
        sub match {
          case Nil => true
          case _   => false
        }
      case Cons(a, as) =>
        sub match {
          case Nil         => true
          case other => startsWith(list, sub) || hasSubsequence(as, sub)
        }
    }
}