package datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => z + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFoldLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def productFoldLeft(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def reverse[T](l: List[T]) = foldLeft[T, List[T]](l, Nil)((z, x) => Cons(x, z))

  def foldRightL[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftR[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def appendFoldRight[T](l: List[T], r: List[T]): List[T] =
    foldRight(l, r)((x, z) => Cons(x, z))

  def flatten[T](ls: List[List[T]]): List[T] =
    foldLeft[List[T], List[T]](ls, Nil)((z, x) => append(z, x))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightL[A, List[B]](l, Nil)((x, z) => Cons(f(x), z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft[A, List[A]](as, Nil)(
      (z, x) => if (f(x)) Cons(x, z) else z
    )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft[List[B], List[B]](map(as)(f), Nil)((z, x) => append(x, z))

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def +(lhs: List[Int], rhs: List[Int]): List[Int] = (lhs, rhs) match {
    case (Cons(l, ls), Cons(r, rs)) => Cons(l + r, List.+(ls, rs))
    case _ => Nil
  }

  def zipWith[A, B, C](lhs: List[A], rhs: List[B])(f: (A, B) => C): List[C] =
    (lhs, rhs) match {
      case (Cons(l, ls), Cons(r, rs)) => Cons(f(l, r), zipWith(ls, rs)(f))
      case _ => Nil
    }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    if (length(sup) < length(sub)) false
    else if (foldLeft(zipWith(sup, sub)(_ == _), true)(_ && _)) true
    else hasSubsequence(tail(sup), sub)
}
