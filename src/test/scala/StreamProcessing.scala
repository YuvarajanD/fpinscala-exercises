import org.specs2.mutable.Specification
import fpinscala.streamprocessing._
import Process._

import scalaz._, Scalaz._

class StreamProcessing extends Specification {

  "Exercise 1" >> {
    val p1: Process[Int, Int] = emitAll(Seq(1, 2, 3))
    val p2: Process[Int, Int] = emitAll(Seq(4, 5, 6))
    val p3 = p1 |> p2
    p3(Stream.empty).toList ==== (4 to 6).toList
  }

  "lifting a function" >> {
    val plusOne = lift((i: Int) => i + 1)
    plusOne((1 to 4).toStream).toList ==== (2 to 5).toList
  }

  "filtering" >> {
    val plusOne = lift((i: Int) => i + 1)
    val evens = filter((i: Int) => i % 2 == 0)
    evens((1 to 10).toStream).toList ==== List(2, 4, 6, 8, 10)
    (plusOne |> evens)((2 to 12).toStream).toList ==== List(4, 6, 8, 10, 12)
  }

  "sum" >> { sum(Stream(1, 2, 3)).toList ==== List(1.0, 3, 6) }
  "take" >> { take(3)((1 to 10).toStream).toList ==== List(1, 2, 3) }
  "drop" >> { drop(3)((1 to 10).toStream).toList ==== (4 to 10).toList }
  "take" >> { take(3)((1 to 10).toStream).toList ==== List(1, 2, 3) }
  "takeWhile" >> { takeWhile((i: Int) => i <= 3)((1 to 10).toStream).toList ==== List(1, 2, 3) }
  "dropWhile" >> { dropWhile((i: Int) => i <= 3)((1 to 10).toStream).toList ==== (4 to 10).toList }

  "count" >> { count(Stream("a", "b", "c")).take(10).toList ==== List(1, 2, 3) }
  "mean" >> { mean(Stream(1, 2, 3)).toList ==== List(1, 1.5, 2) }
  "countWithLoop" >> { countWithLoop(Stream("a", "b", "c")).take(10).toList ==== List(1, 2, 3) }
  "sumWithLoop" >> { sumWithLoop[Int].apply(Stream(1, 2, 3)).toList ==== List(1, 3, 6) }
  "zipped mean" >> { zippedMean(Stream(1, 2, 3)).toList ==== List(1, 1.5, 2) }
  "applicative mean" >> { applicativeMean(Stream(1, 2, 3)).toList ==== List(1, 1.5, 2) }

}