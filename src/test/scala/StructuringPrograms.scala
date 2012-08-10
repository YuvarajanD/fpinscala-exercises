import org.specs2.mutable.Specification
import fpinscala.structuringprograms._
import Exercises._

class StructuringPrograms extends Specification {

  "Exercise 1".txt

  def wider(x: Box, y: Box): Box =
    greaterBy(x, y, _.width)

  def taller(x: Box, y: Box) =
    greaterBy(x, y, _.height)

  "Exercise 2 - absolute takes a function and returns another one where the result is always positive" >> {
    Exercises.absolute((i: Int) => i + 1)(-10) === 9
    Exercises.absolute((i: Int) => i + 1)(10)  === 11
  }
  "Exercise 3 - absolute can be made polymorphic" >> {
    absolute2((s: String) => -s.size + 5)("hello world") === 6
    absolute2((s: String) => -s.size + 3)("fp")          === 1
  }
  "Exercise 4 - divisibleBy" >> {
    divisibleBy(5)(25) === true
    divisibleBy(5)(13) === false
  }
  "Exercise 5 - even with divisibleBy" >> {
    even(5) === false
    even(6) === true
  }
  "Exercise 6 - divisibleBy 3 or divisibleBy 5" >> {
    divisibleBy3Or5(15) === true
    divisibleBy3Or5(20) === true
    divisibleBy3Or5(18) === true
    divisibleBy3Or5(13) === false
  }
  "Exercise 7 - curry" >> {
    curry((_:Boolean) && (_:Boolean))(true)(false) === false
  }
  "Exercise 8 - uncurry" >> {
    uncurry(curry((_:Boolean) && (_:Boolean)))(true, false) === false
  }
  "Exercise 9 - compose" >> {
    compose((_:Int) + 1, (_:Int) * 2)(3) === 7
  }
  "Exercise 10 - 11 - lift3" >> {
    lift3((a: Int, b: Int, c :Int) => a + b + c)((_:Int) + 1, (_:Int) * 2, (_:Int) - 5)(10) === 36
  }
  "Exercise 12 - fib" >> {
    fib(0) === 0               // 0
    fib(1) === 1               // 1
    fib(2) === fib(1) + fib(0) // 1
    fib(3) === fib(2) + fib(1) // 2
    fib(4) === fib(3) + fib(2) // 3
    fib(5) === fib(4) + fib(3) // 5
    fib(6) === fib(5) + fib(4) // 8
    fib(7) === fib(6) + fib(5) // 13
  }
  "Exercise 13 - iterate" >> {
    sqrt(4.0) === 2
    sqrt(5.0) === 2.23606797749979
  }
}