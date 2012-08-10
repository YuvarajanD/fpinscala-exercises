package fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[B, D >: E](f: A => Either[D, B]): Either[D, B]
  def orElse[B >: A, D >: E](b: Either[D, B]): Either[D, B]
  def map2[B, C, D >: E](b: Either[D, B])(f: (A, B) => C): Either[D, C]
}
case class Left[+E](val get: E) extends Either[E, Nothing] {
  def map[B](f: Nothing => B): Either[E, B] = this
  def flatMap[B, D >: E](f: Nothing => Either[D, B]): Either[D, B] = this
  def orElse[B >: Nothing, D >: E](b: Either[D, B]): Either[D, B] = b
  def map2[B, C, D >: E](b: Either[D, B])(f: (Nothing, B) => C): Either[D, C] = this
}
case class Right[+A](val a: A) extends Either[Nothing, A] {
  def map[B](f: A => B): Either[Nothing, B] = Right(f(a))
  def flatMap[B, D >: Nothing](f: A => Either[D, B]): Either[D, B] = f(a)
  def orElse[B >: A, D >: Nothing](b: Either[D, B]): Either[D, B] = this
  def map2[B, C, D >: Nothing](b: Either[D, B])(f: (A, B) => C): Either[D, C] = b.map(f(a, _))
}

sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B]
  def flatMap[B, D >: E](f: A => Validation[D, B]): Validation[D, B]
  def orElse[B >: A, D >: E](b: Validation[D, B]): Validation[D, B]
  def map2[B, C, D >: E](b: Validation[D, B])(f: (A, B) => C): Validation[D, C]
}

case class LeftValidation[+E](list: List[E]) extends Validation[E, Nothing] {
  def map[B](f: Nothing => B): Validation[E, B] = this
  def flatMap[B, D >: E](f: Nothing => Validation[D, B]): Validation[D, B] = this
  def orElse[B >: Nothing, D >: E](b: Validation[D, B]): Validation[D, B] = b
  def map2[B, C, D >: E](b: Validation[D, B])(f: (Nothing, B) => C): Validation[D, C] =
    b match {
      case LeftValidation(l)  => LeftValidation(list ++ l)
      case _                  => this
    }
}
object LeftValidation {
  def apply[E](e: E): LeftValidation[E] = new LeftValidation(List(e))
}
case class RightValidation[+A](a: A) extends Validation[Nothing, A] {
  def map[B](f: A => B): Validation[Nothing, B] = RightValidation(f(a))
  def flatMap[B, D >: Nothing](f: A => Validation[D, B]): Validation[D, B] = f(a)
  def orElse[B >: A, D >: Nothing](b: Validation[D, B]): Validation[D, B] = this
  def map2[B, C, D >: Nothing](b: Validation[D, B])(f: (A, B) => C): Validation[D, C] = b.map(f(a, _))
}


object Either {
  def sequence[A, E](list: List[Either[E, A]]): Either[E, List[A]] =
    traverse(list)(identity)

  def traverse[A, B, E](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    list.foldRight(Right(Nil): Either[E, List[B]]) { (cur, res)  =>
      f(cur).map2(res)(_ :: _)
    }
}

object Validation {
  def sequence[A, E](list: List[Validation[E, A]]): Validation[E, List[A]] =
    traverse(list)(identity)

  def traverse[A, B, E](list: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] =
    list.foldRight(RightValidation(Nil): Validation[E, List[B]]) { (cur, res)  =>
      f(cur).map2(res)(_ :: _)
    }

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!") 
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] = 
    try {
      Right(x / y)
    } catch {
      case e:Exception => Left(e)
    }

}