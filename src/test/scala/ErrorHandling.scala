import org.specs2.mutable.Specification
import fpinscala.errorhandling._
import Option._
import Either._

class ErrorHandling extends Specification {

  "Exercise 1 - option functions" >> {
    Some(1).filter(_ > 1) === None
  }
  "Exercise 2 - variance" >> {
    variance(Seq(1, 2, 3, 4)) === Some(1.25)
  }
  "Exercise 3 - map2" >> {
    map2(Some(1), Some(2))(_+_) === Some(3)
  }
  "Exercise 4 - bothMatch" >> {
    bothMatch(".ab.", "a.b.", "aabc") === Some(true)
  }
  "Exercise 5 - sequence" >> {
    Option.sequence(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3))
  }
  "Exercise 6 - traverse" >> {
    sequence2(List(Some(1), Some(2), Some(3))) === Some(List(1, 2, 3))
  }
  "Exercise 7 - either functions" >> {
    Right(1).map(_ + 2) === Right(3)
  }
  "Exercise 8 - either traverse and sequence" >> {
    Either.sequence(List(Right(1), Right(2), Right(3))) === Right(List(1, 2, 3))
    Either.sequence(List(Right(1), Left(2), Right(3)))  === Left(2)
  }
  "Exercise 9 - validation" >> {
    Validation.sequence(List(RightValidation(1), RightValidation(2), RightValidation(3)))        === RightValidation(List(1, 2, 3))
    Validation.sequence(List(RightValidation(1), LeftValidation("bad"), LeftValidation("code"))) === LeftValidation(List("bad", "code"))
  }
}