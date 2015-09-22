package part1.state


trait RNG { def nextInt: (Int, RNG) }

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val newRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, newRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    if (n < 0) (- (n + 1), r)
    else (n, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i => val mod = i % n
        if (i + n - 1 - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => val (a, r) = f(rng)
      g(a)(r)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(
      a => map(rb)(
        b => f(a, b)))

  def both[A, B](a: Rand[A], b: Rand[B]): Rand[(A, B)] =
    map2(a, b)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  def ints(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))
}

case class State[S, +A](run: S => (A, S))
