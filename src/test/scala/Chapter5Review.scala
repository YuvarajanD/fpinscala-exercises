import org.specs2.mutable.Specification

class Chapter5Review extends Specification  { val txt =
  """
   * "Exercise 2: write a function" - typo on 'function'
   * Exercise 13: small typo 'Stream(1,2,3) starsWith Stream(1,2)'

   5.2 The example given is incorrect:

    |scala> def pair2(i: => Int) = { lazy val j = i; (j, j) }
    |scala> pair2 { println("hi"); 1 + 41 }
    |hi

   It is actually not necessary to use the lazy keyword here so the example is a bit misleading

  """
}
