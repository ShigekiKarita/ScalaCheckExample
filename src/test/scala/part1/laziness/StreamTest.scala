package part1.laziness

import common.ScalaTestCommon


class StreamTest extends ScalaTestCommon {
  def toStream[T](xs: List[T]): Stream[T] =
    xs.foldRight[Stream[T]](Empty)((x, z) => Cons(() => x, () => z))

  "Stream.head & Stream.headOption" should "be" in {
    var i = 0
    val x = Cons[Int](() => {i += 1; i}, () => Stream[Int]())

    // lazy evaluation
    val _ = x.head
    i mustBe 0
    // strict evaluation
    x.headOption
    x.headOption
    i mustBe 2
  }

  "Stream.toList" should "be" in forAll {
    xs: List[Int] => toStream(xs).toList mustBe xs
  }

  "Stream.take & Stream.drop" should "be" in forAll {
    (xs: List[Int], i: Int) => {
      val s = toStream(xs)
      s.take(i).toList ::: s.drop(i).toList mustBe xs
    }
  }
}
