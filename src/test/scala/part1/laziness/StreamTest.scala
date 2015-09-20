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
      s.take(i).toList mustBe xs.take(i)
      s.drop(i).toList mustBe xs.drop(i)
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

  "Stream.ones" should "be" in forAll(lengthGen) {
    i: Int => {
      val expected = List.fill(i)(1)
      Stream.ones.take(i).toList mustBe expected
      Stream.ones.takeWhile(1 ==).take(i).toList mustBe expected
      Stream.ones map (1 +) exists (_ % 2 == 0) mustBe true
      Stream.ones.forAll(1 !=) mustBe false
    }
  }

  "Stream.constant" should "be" in forAll(lengthGen, doubleGen) {
    (i: Int, d: Double) => Stream.constant(d).take(i).toList mustBe List.fill(i)(d)
  }

  "Stream.from" should "be" in forAll(intGen, lengthGen) {
    (i: Int, j: Int) => Stream.from(i).take(j).toList mustBe List.range(i, i + j)
  }

  "Stream.fibs" should "be" in {
    Stream.fibs.take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "Stream.unfold companions" should "be" in forAll(lengthGen, doubleGen, intGen) {
    (len: Int, d: Double, i: Int) => {
      Stream.fibsUnfold.take(len).toList mustBe Stream.fibs.take(len).toList
      Stream.constantUnfold(d).take(len).toList mustBe Stream.constant(d).take(len).toList
      Stream.fromUnfold(i).take(len).toList mustBe Stream.from(i).take(len).toList
      Stream.onesUnfold.take(len).toList mustBe Stream.ones.take(len).toList
    }
  }

  "Stream.unfold methods" should "be" in forAll {
    (xs: List[Int], ys: List[Int], d: Double, i: Int) => {
      val s = toStream(xs)
      val t = toStream(ys)
      s.mapUnfold(d *).toList mustBe xs.map(d *)
      s.takeUnfold(i).toList mustBe xs.take(i)
      s.takeWhileUnfold(i <).toList mustBe xs.takeWhile(i <)
      s.zip(t).toList mustBe xs.zip(ys)
    }
  }

  "Stream.zipAll" should "be" in forAll {
    (xs: List[Int], ys: List[Int]) => {
      val (x, xl) = (toStream(xs), xs.length)
      val (y, yl) = (toStream(ys), ys.length)
      val diffLen = math.abs(xl - yl)
      val expected =
        if (xl > yl) (xs.map(Some(_)), ys.map(Some(_)) ++ List.fill(diffLen)(None))
        else (xs.map(Some(_)) ++ List.fill(diffLen)(None), ys.map(Some(_)))

      val s = x.zipAll(y)
      val result = (s.map(_._1).toList, s.map(_._2).toList)
      result mustBe expected
    }
  }

  "Stream.startWith" should "be" in forAll {
    (xs: List[Int], ys: List[Int]) => {
      val s = toStream(xs)
      val t = toStream(xs)
      s.append(t).startWith(s) mustBe true
    }
  }

  "Stream.tails" should "be" in {
    val result = Stream(1,2,3).tails.map(_.toList).toList
    val expected = List(List(1,2,3), List(2,3), List(3), List())
    result mustBe expected
  }

  "Stream.hasSubsequence" should "be" in forAll {
    (xs: List[Int], ys: List[Int], zs: List[Int]) => {
      val x = toStream(xs)
      val y = toStream(ys)
      val z = toStream(zs)
      (x append y append z).hasSubsequnce(y) mustBe true
    }
  }

  "Stream.scanRight" should "be" in {
    Stream(1,2,3).scanRight(0)(_ + _).toList mustBe List(6,5,3,0)
  }
}
