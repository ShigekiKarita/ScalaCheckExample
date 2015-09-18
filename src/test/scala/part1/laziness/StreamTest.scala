package part1.laziness

import common.ScalaTestCommon


class StreamTest extends ScalaTestCommon {
  def toStream[T](xs: List[T]): Stream[T] =
    xs.foldRight[Stream[T]](Empty)((x, z) => Cons(() => x, () => z))

  "Stream.Cons" should "be" in {
    var i = 0
    val lx = Cons[Int](() => {i += 1; i}, () => Stream[Int]())

    // lazy
    var _0 = lx.head
    i mustBe 0
    var _1 = lx.head
    i mustBe 0

    // strict
    lx.headOption
    i mustBe 1
    lx.headOption
    i mustBe 2
  }

  "Stream.cons" should "be cached" in {
    // lazy evaluation
    var i = 0
    val sx = Stream.cons[Int]({i += 1; i}, Stream[Int]())

    sx.headOption // evaluated once
    i mustBe 1
    sx.headOption // never evaluated twice
    i mustBe 1
  }

  "Stream.empty.headOption" should "be None" in {
    Stream.empty.headOption mustBe None
  }

  "Stream.toList" should "be" in forAll {
    (x: Int, y: Int, z: Int, xs: List[Int]) => {
      Stream(x, y, z).toList mustBe List(x, y, z)
      toStream(xs).toList mustBe xs
    }
  }

  "Stream.take & Stream.drop" should "be" in forAll {
    (xs: List[Int], i: Int) => {
      val s = toStream(xs)
      s.take(i).toList ::: s.drop(i).toList mustBe xs
    }
  }
}
