import java.util.concurrent._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import Par._
import scalaz.Monoid

class PurelyFunctionalParallelism extends Specification with ScalaCheck {
  implicit val executor = Executors.newFixedThreadPool(10)//SingleThreadExecutor//

  "fork always returns the same value" >> prop { i: Int => run(fork(unit(i))).get ==== run(unit(i)).get }.set(workers = 1)

  "it is possible to compute the sum of a list in parallel" >> {
    run(sum(Vector(1, 2, 3, 4))).get === 10
  }

  "it is possible to compute the maximum of a list in parallel" >> {
    run(max(Vector(1, 2, 3, 2, 1))).get === 3
  }

  "parMap must map a function in parallel over the elements of a list" >> {
    run(parMap(List(1, 2, 3, 4))(_ + 1)).get === List(2, 3, 4, 5)
  }

  "parFilter must filter elements of a collection in parallel" >> {
    run(parFilter(List(1, 2, 3, 4))(_ % 2 == 0)).get === List(2, 4)
  }
  "it is possible to spin some parallel computations based on a previous one" >> {
    val filtered = parFilter(List(1, 2, 3, 4))(_ % 2 == 0)
    val (ifTrue, ifFalse) = (sum(Vector(1, 2, 3, 4)), max(Vector(1, 2, 3, 4)))

    run(choice(Par.map(filtered)(_.size % 2 == 0))(ifTrue, ifFalse)).get ==== 10
  }
  /**
   * map(map(y)(g))(f) == map(y)(f compose g)
   *
   * map(g(y))(f)
   * ==>
   * map((f compose g)(y))
   * ==>
   * map(y)(f compose g)
   */


  def sum(as: IndexedSeq[Int]): Par[Int] =
    parallelize(as)(intSumMonoid)

  def max(as: IndexedSeq[Int]): Par[Int] =
    parallelize(as)(intMaxMonoid)

  def parallelize[R : Monoid](as: IndexedSeq[R]): Par[R] =
    if (as.size <= 1) Par.unit(as.headOption getOrElse Monoid[R].zero)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(parallelize(l), parallelize(r))((a, b) => Monoid[R].append(a, b))
    }

  val intSumMonoid = new Monoid[Int] {
    lazy val zero = 0
    def append(i: Int, j: =>Int) = i + j
  }
  val intMaxMonoid = new Monoid[Int] {
    lazy val zero = Int.MinValue
    def append(i: Int, j: =>Int) = if (i > j) i else j
  }
}


object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A](a: Par[A])(implicit s: ExecutorService): Future[A] = a(s)

  def unit[A](a: A): Par[A] =
    (s: ExecutorService) => new Future[A] {
      def isCancelled = false
      def get(timeout: Long, unit: TimeUnit) = a
      def get() = a
      def cancel(mayInterruptIfRunning: Boolean) = false
      def isDone = true
    }

  def async[A](a: =>A) = fork(unit(a))
  def asyncF[A, B](f: A => B) = (a: A) => async(f(a))
  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)

  def delay[A](a: =>Par[A]): Par[A] = (s: ExecutorService) => a(s)

  def fork[A](a: =>Par[A]): Par[A] = {
    (s: ExecutorService) => {
      s.submit(new Callable[A] {
        def call() = a(s).get
      })
    }
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    flatMap(a)((condition: Boolean) => if (condition) ifTrue else ifFalse)

  def chooseN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(a)((i: Int) => choices(i))

  def flatMap[A, B](a: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(a)(choices))

  def join[A](a: Par[Par[A]]): Par[A] =
    (s: ExecutorService) => map(a)(_(s).get)(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(combine: (A, B) => C): Par[C] =
    (s: ExecutorService) => new Future[C] {
      lazy val (fa, fb) = (a(s), b(s))
      def get() = combine(fa.get, fb.get)
      def get(timeout: Long, timeUnit: TimeUnit) = unit(combine(fa.get, fb.get))(s).get(timeout, timeUnit)

      def isCancelled = fa.isCancelled || fb.isCancelled
      def cancel(mayInterruptIfRunning: Boolean) = fa.cancel(mayInterruptIfRunning) && fb.cancel(mayInterruptIfRunning)
      def isDone = fa.isDone && fb.isDone
    }

  def map[A,B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a,_) => f(a))

  def sequence[A](list: List[Par[A]]): Par[List[A]] =
    list.foldLeft(unit(List[A]()))((res, cur) => map2(res, cur)(_ :+ _))

  def parMap[A,B](list: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(list.map(asyncF(f))))

  def parFilter[A](list: List[A])(f: A => Boolean): Par[List[A]] = {
    val options = list.map(a => fork(unit(if (f(a)) Some(a) else None)))
    map(sequence(options))(_.flatten)
  }
}

