package fpinscala.structuringprograms

import annotation.tailrec

case class Box(height: Double, width: Double)

object Exercises {
  def abs(n: Int): Int =
    if (n < 0) -n else n

  def greaterBy(x: Box, y: Box, f: Box => Double): Box =
    if (f(x) > f(y)) x else y

  type Pred[A] = A => Boolean

  def absolute(f: Int => Int): Int => Int =
    (n: Int) => abs(f(n))

  def absolute2[A](f: A => Int): A => Int =
    (n: A) => abs(f(n))

  def even: Pred[Int] =
    divisibleBy(2)

  def divisibleBy(k: Int): Pred[Int] =
    (n: Int) => n % k == 0

  def divisibleBy3Or5: Pred[Int] =
    lift(_ || _, divisibleBy(3), divisibleBy(5))

  def lift[A](f: (Boolean, Boolean) => Boolean,
              g: Pred[A],
              h: Pred[A]): Pred[A] =
    (a: A) => f(g(a), h(a))

  def lift2[A, B, C, D](f: (B, C) => D,
              g: A => B,
              h: A => C): A => D =
    (a: A) => f(g(a), h(a))

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def curry3[A,B,C,D](f: (A, B, C) => D): A => B => C => D =
    (a: A) => (b: B) => (c: C) => f(a, b, c)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def apply[A, B](f: A => B, a: A): B = f(a)
  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B,
                                          h: A => C,
                                          i: A => D): A => E = {
    lift2[A, D => E, D, E](apply, lift2((b: B, c: C) => ((d: D) => f(b, c, d)), g, h), i)
  }

  def fib(n: Int): Int = {
// non tailrec version
//    if      (n == 0) 0
//    else if (n == 1) 1
//    else             fib(n - 1) + fib(n - 2)
    /**
     * @param k current step
     * @param i last Fibonacci number
     * @param j penultimate Fibonacci number
     */
    @tailrec
    def fib1(k: Int, i: Int, j: Int): Int =
      if (k == n) i
      else        fib1(k + 1, i + j, i)

    if (n == 0) 0
    else        fib1(1, 1, 0)
  }

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n // 
    iterateWhile(2.0)(x => x - f(x) / (2 * x), // 
                      x => f(x).abs > 1e-14) // 
  }

  @tailrec
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
    if (!p(a)) a
    else       iterateWhile(f(a))(f, p)
}