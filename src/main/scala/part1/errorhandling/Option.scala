package part1.errorhandling

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _, Some => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def filter(f: A => Boolean): Option[A] = this.map(f) match {
    case Some(true) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    // a.foldRight[Option[List[A]]](Some(Nil))(map2(_, _)(_ :: _))
    traverse(a)(x => x)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
}