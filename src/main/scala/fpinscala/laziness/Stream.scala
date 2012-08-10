package fpinscala.laziness

import Stream._

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None         => z
    }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails.map(_.foldRight(z)(f))

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def toList: List[A] = uncons match {
    case Some((a, rest)) => a :: rest.toList
    case None            => Nil
  }

  def take(n: Int): Stream[A] = uncons match {
    case Some((a, rest)) if n >= 1 => cons(a, rest.take(n - 1))
    case _                         => Stream()
  }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) { case (xs, i) => if (i > 0) xs.uncons.map { case (x, rest) => (x, (rest, i - 1)) } else None }

  def map2[B](f: A => B) =
    unfold(this)(s => s.uncons.map { case (a, rest) => (f(a), rest) })

  def drop(n: Int): Stream[A] = uncons match {
    case Some((a, rest)) if n >= 1 => rest.drop(n - 1)
    case _                         => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    uncons match {
      case Some((a, rest)) if p(a) => cons(a, rest.takeWhile(p))
      case _                       => Stream()
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this)(s => s.uncons.filter { case (a, rest) => (p(a))})

  def zip[B](bs: Stream[B]): Stream[(A, B)] =
    unfold((this, bs)) { s =>
      (s._1.uncons, s._2.uncons) match {
        case (Some((a, rest)), Some((b, rest2))) => Some(((a, b), (rest, rest2)))
        case _                                   => None
      }
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, bs)) { s =>
      (s._1.uncons, s._2.uncons) match {
        case (Some((a, rest)), Some((b, rest2))) => Some(((Some(a), Some(b)), (rest, rest2)))
        case (Some((a, rest)), None)             => Some(((Some(a), None), (rest, Stream())))
        case (None, Some((a, rest)))             => Some(((None, Some(a)), (Stream(), rest)))
        case _                                   => None
      }
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((cur, res) => p(cur) && res)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((cur, res) => if (p(cur)) cons(cur, res.takeWhile(p)) else Stream())

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((cur, res) => cons(f(cur), res))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((cur, res) => if (p(cur)) cons(cur, res.filter(p)) else res.filter(p))

  def append[B >: A](xs: Stream[B]): Stream[B] =
    foldRight(xs)((cur, res) => cons(cur, res))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((cur, res) => f(cur).append(res))

  def startsWith[B >: A](bs: Stream[B]) = Stream.startsWith(this, bs)

  def tails: Stream[Stream[A]] =
    unfold(this)(s => s.uncons.map { case (a, rest) => (s, rest) }).append(Stream(Stream.empty))

  def hasSubsequence[B >: A](s2: Stream[B]): Boolean =
    tails.exists(_.startsWith(s2))

  override def equals(a: Any) = {
    a match {
      case s: Stream[_] => (s.uncons, this.uncons) match {
        case (Some((x, rest1)), Some((y, rest2))) => x == y && rest1 == rest2
        case (None, None)                         => true
        case _                                    => false
      }
      case _ => false
    }
  }

  override def toString = toList.mkString(", ")
}
object Stream {
  def empty[A]: Stream[A] = 
    new Stream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  val ones2: Stream[Int] = unfold(1)(s => Some((1, 1)))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def fibs: Stream[Int] = {
    def next(first: Int, second: Int): Stream[Int] = {
      val fibo = first + second
      cons(fibo, next(second, fibo))
    }
    cons(0, cons(1, next(0, 1)))
  }

  def fibs2: Stream[Int] =
    cons(0, cons(1, unfold((0, 1)) { case (a, b) => Some((a + b, (b, a + b))) }))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => Stream()
    }

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.zipAll(s2).forAll { p =>
      p match {
        case (Some(a), Some(b)) => a == b
        case (Some(a), None)    => true
        case (None, Some(a))    => false
        case (None, None)       => true
      }
    }

}