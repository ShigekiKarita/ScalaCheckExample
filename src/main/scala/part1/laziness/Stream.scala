package part1.laziness

import com.sun.xml.internal.ws.api.streaming.XMLStreamReaderFactory.Default

import scala.annotation.tailrec

trait Stream[+A] {
  import Stream._
  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

  def headOptionFoldRight: Option[A] =
    foldRight[Option[A]](None)((x, z) => Some(x))

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case Empty => Empty
    }

  def takeUnfold(n: Int) = unfold((n, this)) {
    case (i, Cons(h, t)) if i > 0 => Some(h(), (i - 1, t()))
    case _ => None
  }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Cons(h, t) => t().drop(n - 1)
      case Empty => this
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
      case Empty => z
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, z) => if (p(x)) cons(x, z) else empty)

  def takeWhileUnfold(p: A => Boolean) = unfold(this) {
    case Empty => None
    case Cons(h, t) =>
      lazy val head = h()
      if (p(head)) Some(head, t())
      else None
  }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(head, tail) => p(head()) || tail().exists(p)
      case _ => false
    }

  def existsFoldRight(p: A => Boolean): Boolean =
    foldRight(false)(p(_) || _)

  def forAll(p: A => Boolean): Boolean =
    ! exists(! p(_))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, z) => cons(f(x), z))

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, z) => if (p(x)) cons(x, z) else z)

  def append[AA>:A](that: => Stream[AA]): Stream[AA] =
    foldRight(that)(cons(_, _))

  def flatMap[AA>:A](f: AA => Stream[AA]): Stream[AA] =
    foldRight(empty[AA])((x, z) => f(x).append(z))

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(r, rs), Cons(l, ls)) => Some(f(r(), l()), (rs(), ls()))
      case _ => None
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  // smart constructors
  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val headCache = head
    lazy val tailCache = tail
    Cons(() => headCache, () => tailCache)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val c = Cons[A](() => a, () => constant(a))
    c
  }

  def from(n: Int): Stream[Int] = {
    lazy val c = Cons(() => n, () => from(n + 1))
    c
  }

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  val fibsUnfold = unfold((0, 1)) {
    case (a, b) => Some(a, (b, a + b))
  }

  def constantUnfold[T](x: T) =
    unfold(None)(_ => Some(x, None))

  def fromUnfold(i: Int) =
    unfold(i)(x => Some(x, x + 1))

  val onesUnfold = unfold(None)(_ => Some(1, None))
}