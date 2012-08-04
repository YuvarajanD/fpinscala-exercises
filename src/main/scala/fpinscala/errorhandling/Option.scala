package fpinscala.errorhandling
sealed trait Option[+A] {

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


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

  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

}