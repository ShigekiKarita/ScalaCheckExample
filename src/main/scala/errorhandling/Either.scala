package errorhandling

sealed trait Either[+L, +R]
case class Left[+L](value: L)  extends Either[L, Nothing]
case class Right[+R](value: R) extends Either[Nothing, R]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty List!")
    else Right(xs.sum / xs.length)
}