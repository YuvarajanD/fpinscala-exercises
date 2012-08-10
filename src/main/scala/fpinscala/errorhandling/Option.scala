package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B>:A](default: => B): B
  def orElse[B>:A](ob: Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](a: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(a))
  def flatMap[B](f: A => Option[B]): Option[B] = f(a)
  def getOrElse[B>:A](default: => B): B = a
  def orElse[B>:A](ob: Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] = if (f(a)) this else None
}
case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def getOrElse[B>:Nothing](default: => B): B = default
  def orElse[B>:Nothing](ob: Option[B]): Option[B] = ob
  def filter(f: Nothing => Boolean): Option[Nothing] = None
}


object Option {
  case class MyException(msg: String) extends RuntimeException
  def failingFn(i: Int): Int = 
    try {
      if (i > 42) throw MyException("fail!")
      else i + 42
    } catch {
      case MyException(msg) => 42
    }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]) = {
    val xsMean = mean(xs)
    xsMean.flatMap(m => mean(xs.map((x: Double) => math.pow(x - m, 2))))
  }

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = {
    oa flatMap { a =>
      ob map { b =>
        f(a, b)
      }
    }
  }

  def bothMatch(p1: String, p2: String, s: String): Option[Boolean] =
    map2(pattern(p1), pattern(p2))((pattern1, pattern2) => pattern1.matcher(s).matches &&
                                                           pattern2.matcher(s).matches)

  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    list.foldRight(Some(Nil): Option[List[A]])(map2(_, _)(_ :: _))

  def sequence2[A](list: List[Option[A]]): Option[List[A]] =
    traverse(list)(identity)

  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    list.foldRight(Some(Nil): Option[List[B]]) { (cur, res)  =>
      map2(f(cur), res)(_ :: _)
    }

  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

}