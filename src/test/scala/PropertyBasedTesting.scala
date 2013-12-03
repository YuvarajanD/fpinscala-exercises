import org.specs2._
import scalaz.Scalaz._
import scalaz.State
import fpinscala.laziness._
import Stream._

class ScalaCheckPropertyTesting extends mutable.Specification with ScalaCheck {
  "maximum of a list[Int]" >> prop { list: List[Int] =>
    if (list.isEmpty) true
    else if (list.max < Int.MaxValue) list.max < ((list.max + 1) +: list).max
    else true
  }
}

class PropertyBasedTesting extends mutable.Specification {

  type RNG = java.util.Random
  type TestCases = Int
  type FailedCase = String
  type Result = Either[FailedCase, (Status, TestCases)]

  case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[Option[A]]) {
    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f), exhaustive.map(_.map(f)))

    def map2[B, C](genb: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen((sample |@| genb.sample)(f), Stream((exhaustive.toList |@| genb.exhaustive.toList)(f):_*))
  }

  case class Prop(run: (TestCases, RNG) => Result)

  def unit[A](a: =>A) = Gen(State(rng => (rng, a)), cons(Some(a), cons(None, Stream.empty)))

  //def oneOf[A](as: Gen[A]*): Gen[A] = Gen(State(rng => (rng, true)), as.map(_.exhaustive))

  def boolean: Gen[Boolean] = Gen(State(rng => (rng, true)), cons(Some(true), cons(Some(false), Stream.empty)))
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(rng => (rng, start)), Stream((1 to stopExclusive).map(Some.apply):_*))
  /** Generate lists of length n, using the given generator. */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {

    /** sample n values of type A */
    val sample = State { rng: RNG =>
      def values(i: Int, r: RNG, result: List[A]): (RNG, List[A]) = {
        if (i == 0) (r, result)
        else {
          val (r2, a) = g.sample.run(r)
          values(i - 1, r2, a :: result)
        }
      }
      values(n, rng, Nil)
    }
//    def exhaustive: Stream[Option[List[A]]] = {
//      val elements = g.exhaustive.takeWhile(_.isDefined).map(_.get)
//      cons(g.exhaustive.take(n).toList, exhaustive)
//    }

    Gen(sample, Stream.empty)
  }

  sealed trait Status
  case object Unfalsified extends Status
  case object Proven extends Status

  object Prop {
    def apply(n: Int, rng: RNG): Prop = null.asInstanceOf[Prop]
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
      if (i == j) Right((Unfalsified, i))
      else s.uncons match {
        case Some((Some(h), t)) =>
          try { if (f(h)) go(i+1, j, s, onEnd)
                else      Left(h.toString)
          } catch { case e: Exception => Left(buildMsg(h, e)) }

        case Some((None,_)) => Right((Unfalsified,i))
        case None           => onEnd(i)
      }

    go(0, n/3, a.exhaustive, i => Right((Proven, i))) match {
      case Right((Unfalsified,_)) =>
        val rands = randomStream(a)(rng).map(Some(_))
        go(n/3, n, rands, i => Right((Unfalsified, i)))
      case s => s
    }
  }

  def randomStream[A](a: Gen[A])(rng: RNG): Stream[A] = {
    lazy val (rng2, newElement) = a.sample.run(rng)
    Stream.cons(newElement, randomStream(a)(rng2))
  }

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")
}
