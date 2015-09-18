package errorhandling

sealed trait Either[+L, +R] {
  def map[RR](f: R => RR): Either[L, RR] =
    this match {
      case Right(v) => Right(f(v))
      case Left(v) => Left(v)
    }

  def flatMap[LL>:L, RR](f: R => Either[LL, RR]): Either[LL, RR] =
    this match {
      case Right(v) => f(v)
      case Left(v) => Left(v)
    }

  def getOrElse[RR>:R](r: => RR): RR =
    this match {
      case Right(v) => v
      case Left(_) => r
    }

  def orElse[LL>:L, RR>:R](l: => Either[LL, RR]): Either[LL, RR] =
    this match {
      case Right(v) => Right(v)
      case Left(_) => l
    }

  def map2[LL>:L, S, T](s: Either[LL, S])(f: (R, S) => T): Either[LL, T] =
    for {
      r  <- this
      s_ <- s
    } yield f(r, s_)
}

case class Left[+L](value: L)  extends Either[L, Nothing]
case class Right[+R](value: R) extends Either[Nothing, R]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty List!")
    else Right(xs.sum / xs.length)

  def traverse[L, R, S](rs: List[R])(f: R => Either[L, S]): Either[L, List[S]] =
    rs match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def sequence[L, R](es: List[Either[L, R]]): Either[L, List[R]] =
    traverse(es)(identity)
}