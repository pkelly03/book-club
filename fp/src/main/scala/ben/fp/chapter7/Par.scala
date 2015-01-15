package ben.fp.chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.language.postfixOps


object Par {

  type Par[A] = ExecutorService => Future[A]

  type TimedPar[A] = ExecutorService => TimedFuture[A]

  type CurrentTime = () => Long

  def now[A]: Long = System.currentTimeMillis()

  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def timedUnit[A](a: Future[A]): TimedPar[A] =
    (es: ExecutorService) => TimedFuture(a)

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => UnitFuture(f(a(es).get))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    (es: ExecutorService) => {
      choiceN(map2(unit(), cond)((a, b) => if (b) 0 else 1))(List(t, f))(es)
    }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    (es: ExecutorService) => {
      map(n) { i => choices(i)(es)}(es).get
    }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map2WithTimeout[A, B, C](a: TimedPar[A], b: TimedPar[B], timeout: Long, units: TimeUnit)(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val (elapsed, af) = a(es).getWithTimeout(timeout, units)
      val (e2, bf) = b(es).getWithTimeout(timeout - elapsed, units)
      UnitFuture(f(af, bf))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((b, a) => map2(b, a)(_ :: _))

  def sequence_1st_attempt[A](ps: List[Par[A]]): Par[List[A]] =
    (es: ExecutorService) => {
      UnitFuture(ps.foldRight(List.empty[A])((b, a) => b(es).get +: a))
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    sequence(ps.map(asyncF(f)))


  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    val toList: A => List[A] = (a: A) => if (f(a)) List(a) else Nil

    map(sequence(as.map(asyncF(toList))))(_.flatten)
  }

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))


  case class UnitFuture[A](get: A) extends Future[A] {

    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  case class TimedFuture[A](a: Future[A]) extends Future[A] {

    private val ref: AtomicReference[Option[A]] = new AtomicReference(None)

    private def value = ref.get()

    private def getValue: Option[A] = ref.get

    private def setValue(a: A): A = value getOrElse {
      ref.set(Some(a))
      value get
    }

    private def timed(timeout: Long, units: TimeUnit)(future: => A): (Long, A) = {
      val start = now
      val v = future
      (now - start) -> v
    }

    override def isDone = value.isDefined

    override def get(timeout: Long, unit: TimeUnit): A = setValue(a.get(timeout, unit))

    override def get: A = setValue(a.get(5, TimeUnit.SECONDS))

    def getWithTimeout(timeout: Long, units: TimeUnit): (Long, A) = timed(timeout, units)(get(timeout, units))

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = a.cancel(mayInterruptIfRunning)

    override def isCancelled: Boolean = a.isCancelled
  }

}
