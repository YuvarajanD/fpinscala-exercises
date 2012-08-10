import org.specs2.matcher.{Expectable, Matcher}
import org.specs2.mutable.Specification
import RNG._

class PurelyFunctionalState extends Specification  {

  "Exercise 1 - generate a random positive integer" >> {
    rng.positiveInt._1 must be_>=(0)
  }
  "Exercise 2 - generate a random positive double" >> {
    rng.nextDouble._1 must be_>=(0.0)
  }
  "Exercise 3 - generate combinations" >> {
    val ((d1, d2, _), _) = rng.double3
    d1 must not(be_==(d2))
  }
  "Exercise 4 - list of ints" >> {
    rng.ints(3)._1 must have size(3)
  }
  "Exercise 5 - unit" >> {
    val u = unit(2)
    val (value, r) = u(rng)
    value === 2
    u(r)._1 === 2
  }
  "Exercise 6 && 7 - map for generating ints between 0 and 10" >> {
    val ints10 = RNG.map((_:RNG).nextInt)((i: Int) => math.abs(i) % 11)(rng)
    ints10._1 must be_<=(10)
  }
  "Exercise 8 - zip for combinations" >> {
    rng.doubleInt2._1
    rng.intDouble2._1
    ok
  }
  "Exercise 9 - sequence" >> {
    sequence[Int](List((_:RNG).nextInt, (_:RNG).nextInt, (_:RNG).nextInt))(rng)._1 must have size(3)
  }
}

trait RNG {
  def positiveInt: (Int, RNG) = {
    val (i, rng) = nextInt
    (math.abs(i), rng)
  }
  def nextInt: (Int, RNG)
  def nextDouble: (Double, RNG) = {
    val (i, rng) = positiveInt
    (i.toDouble / Int.MaxValue, rng)
  }

  def intDouble: ((Int, Double), RNG) = {
    val (i, r1) = nextInt
    val (d, r2) = r1.nextDouble
    ((i, d), r2)
  }

  def doubleInt: ((Double, Int), RNG) = {
    val (i, r1) = nextInt
    val (d, r2) = r1.nextDouble
    ((d, i), r2)
  }

  def intDouble2 = RNG.zip((_:RNG).nextInt, (_:RNG).nextDouble)(this)
  def doubleInt2 = RNG.zip((_:RNG).nextDouble, (_:RNG).nextInt)(this)

  def double3: ((Double, Double, Double), RNG) = {
    val (d1, r1) = nextDouble
    val (d2, r2) = r1.nextDouble
    val (d3, r3) = r2.nextDouble
    ((d1, d2, d3), r3)
  }
  def ints(n: Int): (List[Int], RNG) = {
    require(n >= 0)
    if (n == 0) (Nil, this)
    else        {
      val (i, r) = nextInt
      val (rest, r2) = r.ints(n - 1)
      (i :: rest, r2)
    }
  }
}

object RNG {
  def rng = simple(0)

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  def unit[A](a: A): RNG => (A, RNG) =
    (r: RNG) => (a, r)

  def map[A,B](a: RNG => (A, RNG))(f: A => B): RNG => (B, RNG) =
    (r: RNG) => {
      val (value, r2) = a(r)
      (f(value), r2)
    }

  def zip[A,B](a: RNG => (A, RNG), b: RNG => (B, RNG)): RNG => ((A, B), RNG) = {
    (r: RNG) => {
      val ((value1, r1), (value2, r2)) = (a(r), b(r))
      ((value1, value2), r1)
    }
  }

  def sequence[A](as: List[RNG => (A, RNG)]): RNG => (List[A], RNG) =
    as.foldLeft((r: RNG) => (List[A](), r)) { (res, cur) =>
      (r: RNG) => {
        val (list, r2) = res(r)
        val (a, r3) = cur(r2)
        (list :+ a, r3)
      }
    }

}