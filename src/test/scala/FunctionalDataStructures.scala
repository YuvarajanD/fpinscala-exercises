import annotation.tailrec
import fpinscala._
import datastructures._
import org.specs2.mutable.Specification
import List._
import Tree._

class FunctionalDataStructures extends Specification {

  "Exercise 1 - match expressions" >> {
    matchingExample === 3
  }
  "Exercise 2 - tail" >> {
    def tail[A](list: List[A]): List[A] =
      (list: @unchecked)  match {
        case Cons(a, as) => as
      }
    tail(List(1, 2, 3)) === List(2, 3)
  }
  "Exercise 3 - drop" >> {
    tail(List(1, 2, 3)) === List(2, 3)
  }
  "Exercise 4 - dropWhile" >> {
    dropWhile(List(1, 2, 3))(_ < 2) === List(2, 3)
  }
  "Exercise 5 - setHead" >> {
    setHead(List(1, 2, 3))(4) === List(4, 2, 3)
  }
  "Exercise 6 - init" >> {
    init(List(1, 2, 3)) === List(1, 2)
  }
  "Exercise 7 - foldRight" >> {
    foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _) === 6
    1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)      === 6
    1 + 2 + foldRight(Cons(3, Nil), 0)(_ + _)           === 6
    1 + 2 + 3 + foldRight(List[Int](), 0)(_ + _)        === 6
    1 + 2 + 3 + 0                                       === 6
  }
  "Exercise 8 - product with foldRight" >> {
    product2(List(1, 2, 3)) === 6
    product2(List(1, 0, 3)) === 0
  }
  "Exercise 9 - foldRight as data definition" >> {
    foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)) === List(1, 2, 3)
  }
  "Exercise 10 - length with foldRight" >> {
    List.length(List(1, 2, 3)) === 3
  }
  "Exercise 11 - foldLeft" >> {
    foldLeft(List(1, 2, 3), 2)(_ + _) === scala.List(1, 2, 3).foldLeft(2)(_ + _)
  }
  "Exercise 12 - length with foldLeft" >> {
    List.length2(List(1, 2, 3)) === 3
  }
  "Exercise 13 - reverse of a list" >> {
    reverse(List(1, 2, 3)) === List(3, 2, 1)
  }
  "Exercise 14 - foldRight <=> foldLeft" >> {
    foldRight2(List(1, 2, 3), 0)(_ + _) === 6
    foldLeft2(List(1, 2, 3), 0)(_ + _)  === 6
  }
  "Exercise 15 - append" >> {
    List.append(List(1, 2, 3), 4) === List(1, 2, 3, 4)
  }
  "Exercise 16 - concatenate" >> {
    List.concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) ===
      List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
  "Exercise 17 - plusOne" >> {
    List.plusOne(List(1, 2, 3)) === List(2, 3, 4)
  }
  "Exercise 18 - asString" >> {
    List.asString(List(1, 2, 3)) === List("1.0", "2.0", "3.0")
  }
  "Exercise 19 - map" >> {
    List.map(List(1, 2, 3))(_ + 1) === List(2, 3, 4)
  }
  "Exercise 20 - map" >> {
    List.filter(List(1, 2, 3))(isOdd) === List(1, 3)
  }
  "Exercise 21 - flatMap" >> {
    List.flatMap(List(1, 2, 3))(i => List(1, i)) === List(1, 1, 1, 2, 1, 3)
  }
  "Exercise 22 - filter with flatMap" >> {
    List.filter2(List(1, 2, 3))(isOdd) === List(1, 3)
  }
  "Exercise 23 - zipAdd" >> {
    List.zipAdd(List(1, 2, 3), List(1, 2, 3)) === List(2, 4, 6)
  }
  "Exercise 24 - zip" >> {
    List.zip(List(1, 2, 3), List(1, 2, 3))(_ + _) === List(2, 4, 6)
  }
  "Exercise 25 - hasSubsequence" >> {
    List.hasSubsequence(List(1), List(0))                   === false
    List.hasSubsequence(List(1, 2), List(2))                === true
    List.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)) === true
    List.hasSubsequence(List(3, 4, 5), List(3, 5))          === false
  }
  "Exercise 26 - tree size" >> {
    Tree.size(Branch(Leaf(1), Leaf(2))) === 2
  }

  //  / \
  // 1
  //   /  \
  //  1
  //      /  \
  //      2  5
  val tree1 = Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(2), Leaf(5))))

  "Exercise 27 - tree maximum" >> {
    Tree.maximum(tree1) === 5
  }
  "Exercise 28 - tree depth" >> {
    Tree.depth(tree1) === 3
  }
  "Exercise 29 - tree map" >> {
    Tree.map(tree1)(_ + 1) === Branch(Leaf(2), Branch(Leaf(2), Branch(Leaf(3), Leaf(6))))
  }
  "Exercise 30 - tree generalization" >> {
    Tree.size2(Branch(Leaf(1), Leaf(2))) === 2
    Tree.maximum2(tree1)                 === 5
    Tree.depth2(tree1)                   === 3
    Tree.map2(tree1)(_ + 1)              === Branch(Leaf(2), Branch(Leaf(2), Branch(Leaf(3), Leaf(6))))

    Tree.size3(Branch(Leaf(1), Leaf(2))) === 2
    Tree.maximum3(tree1)                 === 5
    Tree.depth3(tree1)                   === 3
    Tree.map3(tree1)(_ + 1)              === Branch(Leaf(2), Branch(Leaf(2), Branch(Leaf(3), Leaf(6))))
  }

  def sum[A : Numeric](list: List[A]): A = {
    @tailrec def sum1(ls: List[A], result: A): A =
      ls match {
        case Nil         => result
        case Cons(x, xs) => sum1(xs, implicitly[Numeric[A]].plus(x, result))
      }

    sum1(list, implicitly[Numeric[A]].zero)
  }

  def isOdd = (_:Int) % 2 == 1
}
