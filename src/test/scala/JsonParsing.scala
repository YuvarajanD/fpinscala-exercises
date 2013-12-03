import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class JsonParsing extends Specification with ScalaCheck { val txt =
  """
   ParserOpts implicits page 6
   The | method doesn't have a byname parameter while the 'or' method has
   The self variable is not defined.

"We introduced a type ParseError earlier, but so far we haven't chosen any functions for
 the API of ParseError and our algebra doesn't have any ways of letting the programmer control what errors are reported. This seems like a limitation given that we'd like meaningful error messages from our parsers. Can you do something about it?"

>> meaning "can you define those operations on the ParseError type"? On a trait?
"What does it mean for a parser to succeed?"

    |Try to come up with a set of laws to specify your algebra. You don't necessarily need the laws to be complete, just write down some laws that you expect should hold for any Parsers implementation

>> give an example of such law, for example sizeOf with and. Is it law on operators or law on types like ParseError

Laws:

 * sizeOf(p1 andThen p2) >= sizeOf(p1) andThen sizeOf(p2)

EXERCISE:

 * p andThen success(a) = (p, a)
 * p andThen (p2 andThen p3).flatten = ((p andThen p2) andThen p3)

EXERCISE 6: or is associative

Errors section: give a hint like "how to attach an error message to a parser?"


    |We've used these primitives to define a number of combinators like map, map2 , flatMap, many, and many1.
    |

seq ==> flatMap???? -- it's only introduced p.25

   != primitives on p.28

  """

}
trait Parsers[Parser[+_], ParseError] { self =>

  def char(c: Char): Parser[Char]                      = string(c.toString) map (_.charAt(0))
  def count(c: Char): Parser[Int]                      = sizeOf(zeroOrMore(char(c)))
  def countAtLeastOne(c: Char): Parser[Int]            = sizeOf(oneOrMore(char(c)))
  def countPair(a: Char, b: Char): Parser[(Int, Int)]  = count(a) andThen count(b)
  def zeroOrMore[A](p: Parser[A]): Parser[List[A]]
  def oneOrMore[A](p: Parser[A]): Parser[List[A]] = map2(p, zeroOrMore(p))(_ :: _)
  def andThen[A, B](p: Parser[A], p2: =>Parser[B]): Parser[(A, B)] =
    map2[A, B, (A, B)](p, p2)((a: A, b: B) => (a, b))
  def sizeOf[A](p: Parser[List[A]]): Parser[Int] = map(p)((l: List[A]) => l.size)
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => success(f(a)))
  def slice[A](p: Parser[A]): Parser[String]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many1(p))(_ :: _)

  def success[A](a: A): Parser[A]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): operators[String] = new operators(f(a))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit class operators[A](p: Parser[A]) {
    def |[B >: A](p2: =>Parser[B]): Parser[B] = p | p2
    def or[B >: A](p2: =>Parser[B]): Parser[B] = self.or(p, p2)
    def andThen[B](p2: =>Parser[B]): Parser[(A, B)] = self.andThen(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }
}