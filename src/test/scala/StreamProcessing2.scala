import org.specs2.mutable.Specification

import scalaz._, Scalaz._
import scalaz.stream.Process
import Process._

class StreamProcessing2 extends Specification {

  "Exercise 1" >> {
    val p1 = emitAll(Seq(1, 2, 3))
    val p2 = emitAll(Seq(4, 5, 6))
    val p3 = p1 |> p2
    p3(Stream.empty).toList ==== (4 to 6).toList
  }

  "zipped mean " >> {
    def sum: Process1[Double, Double] = {
      def go(acc: Double): Process1[Double, Double] =
        Process.await(Get[Double])((d: Double) => Emit(Seq(d + acc), go(d + acc)))
      go(0.0)
    }

    def count[T]: Process1[T, Int] = {
      def go(i: Int): Process1[T, Int] =
        Process.await(Get[T])((t: T) => Emit(Seq(i), go(i + 1)))
      go(1)
    }

    def zippedMean: Process1[Double, Double] =
     (sum zipWith count) { case (s, c) => s / c }

    sum(Stream(1, 2, 3, 4)).toList ==== List(1, 3, 6, 10)
    count(Stream(1, 2, 3, 4)).toList ==== List(1, 2, 3, 4)

    (count[Double] zip sum).apply(Stream(1, 2, 3, 4)) ====
     count[Double](Stream(1, 2, 3, 4)).zip(sum(Stream(1, 2, 3, 4)))

    zippedMean(Stream(1, 2, 3, 4)).toList ==== List(1, 1.5, 2, 2.5)

  }
}