package part1.laziness

import common.ScalaTestCommon


class StreamTest extends ScalaTestCommon {
  def toStream[T](xs: List[T]): Stream[T] =
    xs.foldRight[Stream[T]](Empty)((x, z) => Cons(() => x, () => z))

  "Stream.Cons" should "be" in {
    var i = 0
    val lx = Cons[Int](() => {i += 1; i}, () => Stream[Int]())

    // lazy
    val _0 = lx.head
    i mustBe 0
    val _1 = lx.head
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

  "Stream.foldRight" should "be" in forAll {
    xs: List[Int] => {
      val s = toStream(xs)
      val result = s.foldRight(Stream.empty[Int])(Stream.cons(_, _))
      result.toList mustBe xs.foldRight(List[Int]())(_ :: _)
    }
  }

  "Stream.takeWhile" should "be" in forAll {
    (xs: List[Int], i: Int) => {
      val s = toStream(xs)
      val result = s.takeWhile(i <).toList
      result mustBe xs.takeWhile(i <)
    }
  }

  "Stream.exists" should "be" in forAll {
    (xs: List[Int], ys: List[Int], i: Int) => {
      val s = toStream(xs ++ (i :: ys))
      s.exists(i ==) mustBe true
      s.takeWhile(i !=).exists(i ==) mustBe false
    }
  }

  "Stream.forAll" should "be" in forAll {
    (xs: List[Int], i: Int) => {
      val s = toStream(xs).takeWhile(i <)
      s.forAll(i <) mustBe true
    }
  }

  "Stream.foldRight children" should "be" in forAll {
    (xs: List[Int], ys: List[Int], i: Int) => {
      val s = toStream(xs)
      val t = toStream(ys)
      s.takeWhile(i <).toList mustBe s.takeWhileFoldRight(i <).toList
      s.exists(i <) mustBe s.existsFoldRight(i <)
      s.headOption mustBe s.headOptionFoldRight
      s.map(i *).toList mustBe xs.map(i *)
      s.filter(i <).toList mustBe xs.filter(i <)
      s.append(t).toList mustBe xs ++ ys
      s.flatMap(Stream(_, i)).toList mustBe xs.flatMap(List(_, i))
    }
  }

}
