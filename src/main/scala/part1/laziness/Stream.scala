package part1.laziness

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n == 0) Empty
    else this match {
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case Empty => Empty
    }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Cons(h, t) => t().drop(n - 1)
      case Empty => this
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
}