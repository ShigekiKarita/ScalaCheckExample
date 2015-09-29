package part2.paralellism

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

import scala.collection.immutable.Nil


object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def unit[A](a: => A): Par[A] = {
    (es: ExecutorService) => UnitFuture(a)
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isDone: Boolean = true
  }

  def map2[L, R, F](l: Par[L], r: Par[R])(f: (L, R) => F): Par[F] =
    (es: ExecutorService) => UnitFuture(f(l(es).get, r(es).get))

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def sum(xs: Seq[Int]): Par[Int] = {
    if (xs.length <= 1)
      unit(xs.headOption.getOrElse(0))
    else xs.splitAt(xs.length / 2) match {
      case (l, r) => map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }
  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parSort(parList: Par[Seq[Int]]): Par[Seq[Int]] =
    map(parList)(_.sorted)
  
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((x, z) => map2(x, z)(_ :: _))
  
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(ps map asyncF(f)))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(sequence(
      as map asyncF(a => if(f(a)) List(a) else Nil)
    ))(_.flatten)

  def equal[A](es: ExecutorService)(lhs: Par[A], rhs: Par[A]): Boolean =
    lhs(es).get == rhs(es).get
}
