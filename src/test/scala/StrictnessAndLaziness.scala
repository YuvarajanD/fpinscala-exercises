import org.specs2.mutable.Specification
import fpinscala.laziness._
import Stream._

class StrictnessAndLaziness extends Specification  {

  "Exercise 1 - stream to list" >> {
    Stream(1, 2, 3).toList === List(1, 2, 3)
  }
  "Exercise 2 - take" >> {
    Stream(1, 2, 3).take(2) === Stream(1, 2)
    Stream(1, 2, 3).take(3) === Stream(1, 2, 3)
    Stream(1, 2, 3).take(4) === Stream(1, 2, 3)
  }
  "Exercise 3 - takeWhile" >> {
    Stream(1, 2, 3, 4).takeWhile(_ < 3) === Stream(1, 2)
  }
  "Exercise 4 - forAll" >> {
    Stream(1, 2, 3, 4).forAll(_ > 0) === true
    Stream(1, 2, 3, 4).forAll(_ < 2) === false
  }
  "Exercise 5 - takeWhile2" >> {
    Stream(1, 2, 3, 4).takeWhile2(_ < 3) === Stream(1, 2)
  }
  "Exercise 6 - map, filter, append, flatMap" >> {
    Stream(1, 2, 3, 4).map(_ + 1)           === Stream(2, 3, 4, 5)
    Stream(1, 2, 3, 4).filter(_ % 2 == 0)   === Stream(2, 4)
    Stream(1, 2).append(Stream(3, 4))       === Stream(1, 2, 3, 4)
    Stream(1, 2).flatMap(i => Stream(i, i)) === Stream(1, 1, 2, 2)
  }
  "Exercise 7 - constant" >> {
    constant(3).take(5) === Stream(3, 3, 3, 3, 3)
  }
  "Exercise 8 - from" >> {
    from(3).take(5) === Stream(3, 4, 5, 6, 7)
  }
  "Exercise 9 - fibs" >> {
    fibs.take(7) === Stream(0, 1, 1, 2, 3, 5, 8)
  }
  "Exercise 10 - unfold" >> {
    unfold(0)(s => if (s < 4) Some((s, s + 1)) else None) === Stream(0, 1, 2, 3)
  }
  "Exercise 11 - unfold for fibs, from, constant, ones" >> {
    ones2.take(5)        === Stream(1, 1, 1, 1, 1)
    constant2(3).take(5) === Stream(3, 3, 3, 3, 3)
    from2(3).take(5)     === Stream(3, 4, 5, 6, 7)
    fibs2.take(7)        === Stream(0, 1, 1, 2, 3, 5, 8)
  }
  "Exercise 12 - unfold for map, take, takeWhile, zip, zipAll" >> {
    Stream(1, 2, 3).take2(2)             === Stream(1, 2)
    Stream(1, 2, 3, 4).takeWhile3(_ < 3) === Stream(1, 2)
    Stream(1, 2, 3, 4).map2(_ + 1)       === Stream(2, 3, 4, 5)
    Stream(1, 2, 3, 4).zip(Stream(4, 3)) === Stream((1, 4), (2, 3))
    Stream(1, 2, 3, 4).zipAll(Stream(4, 3)) === Stream((Some(1), Some(4)), (Some(2), Some(3)), (Some(3), None), (Some(4), None))
  }
  "Exercise 13 - startsWith" >> {
    Stream(1, 2, 3).startsWith(Stream(1,2)) === true
  }
  "Exercise 14 - hasSubsquence with tails" >> {
    Stream(1, 2, 3).tails === Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream.empty)

    Stream(1)            .hasSubsequence(Stream(0))       === false
    Stream(1, 2)         .hasSubsequence(Stream(2))       === true
    Stream(1, 2, 3, 4, 5).hasSubsequence(Stream(2, 3, 4)) === true
    Stream(3, 4, 5)      .hasSubsequence(Stream(3, 5))    === false
  }
  "Exercise 15 - scanRight" >> {
    Stream(1,2,3).scanRight(0)(_ + _) === Stream(6, 5, 3, 0)
  }
}