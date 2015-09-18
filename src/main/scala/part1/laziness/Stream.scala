package part1.laziness

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
    if (n == 0) Empty
    else this match {
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case Empty => Empty
    }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
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
      case Empty => Empty
      case Cons(h, t) =>
        if (p(h())) Cons(h, () => t().takeWhile(p))
        else Empty
    }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, z) => if (p(x)) cons(x, z) else empty)

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

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, z) => if (p(x)) cons(x, z) else z)

  def append[AA>:A](that: => Stream[AA]): Stream[AA] =
    foldRight(that)(cons(_, _))

  def flatMap[AA>:A](f: AA => Stream[AA]): Stream[AA] =
    foldRight(empty[AA])((x, z) => f(x).append(z))
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
}