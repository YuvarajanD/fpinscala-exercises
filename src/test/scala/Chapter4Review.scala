import org.specs2.mutable.Specification

class Chapter4Review extends Specification  { val txt =
  """
    * I don't find the first example about RT with exceptions very clear if compared with the first explanation in the book

      I would propose to use a more similar example:

      var x = 0

      val y = try {
        val e = error("here")
        x = { e; 1 }
      } finally {
        y
      }

      Then, if you replace e with its value where e is used, you will get a different value for y.

      Also, introducing a new exception class (MyException) for this example doesn't bring anything: sys.error or new Exception
      would be enough

    * 4.2 there is a typo in the first 'mean' definition
      "def mean(xs: Seq[Double]): Option[Double] =" should be
      "def mean(xs: Seq[Double]): Double ="

    * Exercise 7: The Either methods don't have correct type parameters:

      def map[B](f: A => B): Either[E, B]
      def flatMap[B, D >: E](f: A => Either[D, B]): Either[D, B]
      def orElse[B >: A, D >: E](b: Either[D, B]): Either[D, B]
      def map2[B, C, D >: E](b: Either[D, B])(f: (A, B) => C): Either[D, C]

    It would also be nicer to have spaces for types constraints:

       def orElse[B>:A]

    should be

       def orElse[B >: A]

    * other thought: dealing with covariance is hard for beginners so I would make the datastructures invariant for all the exercises

  """

}
