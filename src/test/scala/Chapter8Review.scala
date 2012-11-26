import org.specs2.mutable.Specification

class Chapter8Review extends Specification  {
  """
  typo: REFININING THE PRIMITIVES

    | where is 'randomStream' defined?
    |
    |  Here's my definition:
    |
    |    def randomStream[A](a: Gen[A])(rng: RNG): Stream[A] = {
    |      lazy val (rng2, newElement) = a.sample.run(rng)
    |      Stream.cons(newElement, randomStream(a)(rng2))
    |    }
    |
  """

}
