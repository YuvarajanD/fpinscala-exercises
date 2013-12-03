package fpinscala.streamprocessing

import Process._
import scalaz._, Scalaz._

trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Emit(h, t)            => h.toStream append t(s)
    case Halt()                => Stream()
    case Await(recv, fallback) => s match {
      case h #:: t => recv(h)(t)
      case _       => fallback(s) // Stream is empty
    }
  }

  def map[O2](f: O => O2): Process[I,O2] = this match {
    case Halt()          => Halt()
    case Emit(h, t)      => Emit(h map f, t map f)
    case Await(recv, fb) => Await(recv map (_ map f), fb map f)
  }

  def ++(p: Process[I,O]): Process[I,O] = this match {
    case Halt()          => p
    case Emit(h, t)      => emitAll(h, t ++ p)
    case Await(recv, fb) => Await(recv map (_ ++ p), fb ++ p)
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt()     => Halt()
    case Emit(h, t) =>
      if (h.isEmpty) t flatMap f
      else f         (h.head) ++ emitAll(h.tail, t).flatMap(f)
    case Await(recv,fb) => Await(recv map (_ flatMap f), fb flatMap f)
  }

  def |>[O2](p2: Process[O, O2]): Process[I, O2] = this match {
    case Halt()          => Halt()
    case Emit(h, t)      => p2 feed h match {
      case Halt()        => Halt()
      case Emit(h2, t2)  => Emit(h2, t |> t2)
      case a2            => t |> a2
    }
    case Await(recv, fb) => Await(recv map (_ |> p2), fb |> p2)
  }

  def feed(in: Seq[I]): Process[I, O] = this match {
    case Halt()          => Halt()
    case Emit(h, t)      => Emit(h, t feed in)
    case Await(recv, fb) => try { (in map recv).reduce(_ ++ _) } catch { case t: Throwable => fb }
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt()          => go(this)
      case Await(recv, fb) => Await(recv andThen go, fb)
      case Emit(h, t)      => Emit(h, go(t))
    }
    go(this)
  }

  def zip[O2](p2: Process[I, O2]): Process[I, (O, O2)] = this match {
    case Halt()     => Halt()
    case Emit(h, t) => p2 match {
      case Halt()         => Halt()
      case Emit(h2, t2)   => Emit(h zip h2, t zip t2)
      case Await(r2, fb2) => t zip p2
    }
    case Await(r, fb) => p2 match {
      case Halt()         => Halt()
      case Emit(h2, t2)   => this zip t2
      case Await(r2, fb2) => Await((i: I) => r(i) zip r2(i), fb zip fb2)
    }

  }

}

object Process {
  def emitAll[I, O](head: Seq[O], tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    tail match {
      case Emit(h2, tl) => Emit(head ++ h2, tl)
      case _            => Emit(head, tail)
    }

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    emitAll(Stream(head), tail)

  implicit def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] =
    new Monad[({ type f[x] = Process[I, x]})#f] {
      def point[O](o: => O): Process[I, O] = emit(o)
      def bind[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p flatMap f
    }

  def lift[I, O](f: I => O): Process[I, O] =
    Await((i: I) => emit(f(i), lift(f)))

  def filter[I](f: I => Boolean): Process[I,I] =
    Await[I, I](i => if (f(i)) emit(i) else Halt()).repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await((d: Double) => emit(d + acc, go(d + acc)))
    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    def go(n: Int): Process[I, I] =
     if (n == 0) Halt() else Await((i: I) => emit(i) ++ go(n - 1))
    go(n)
  }

  def drop[I](n: Int): Process[I, I] = {
    def go(n: Int): Process[I, I] =
      if (n == 0) lift(identity) else Await((i: I) => go(n - 1))
    go(n)
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    Await((i: I) => if (f(i)) emit(i) ++ takeWhile(f) else emitAll(Seq()))

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    Await((i: I) => if (f(i)) dropWhile(f) else emit(i) ++ lift(identity))

  def count[T]: Process[T, Int] = {
    def go(i: Int): Process[T, Int] =
      Await((t: T) => emit(i, go(i + 1)))
    go(1)
  }

  def mean: Process[Double, Double] = {
    def go(totalAndNumber: (Double, Double)): Process[Double, Double] = {
      val (total, number) = totalAndNumber
      Await((d: Double) => emit((d + total) / number, go((d + total, number + 1))))
    }
    go((0.0, 1))
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I,O] =
    Await((i: I) => f(i, z) match { case (o, s2) => emit(o, loop(s2)(f)) })

  def countWithLoop[T]: Process[T, Int] = loop(1) { case (t, i) => (i, i + 1) }

  def sumWithLoop[T : Numeric]: Process[T, T] = {
    val n = implicitly[Numeric[T]]
    loop(n.zero) { case (t, i) => (n.plus(t, i), n.plus(t, i)) }
  }

  def zippedMean: Process[Double, Double] =
    (sum zip count[Double]).map { case (s, c) => s / c }

  def applicativeMean: Process[Double, Double] =
    (sum |@| count)((s, c) => s / c)
}

case class Emit[I, O](head: Seq[O], tail: Process[I, O] = Halt[I, O]())                   extends Process[I,O]
case class Await[I, O](recv: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]())  extends Process[I,O]
case class Halt[I, O]() extends Process[I, O]