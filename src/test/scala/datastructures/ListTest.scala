package datastructures

import testutils.ScalaTestCommon
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import scala.annotation.tailrec


class ListTest extends ScalaTestCommon {

  val genNil = Gen.const(Nil)
  val genNonNil = for {
    x  <- arbitrary[Int]
    xs <- genList
  } yield Cons(x, xs)

  def genList: Gen[List[Int]] = Gen.oneOf[List[Int]](genNil, genNonNil)

  // Generator of my List[T]
  implicit def arbList[T](implicit a: Arbitrary[T]): Arbitrary[List[T]] =
    Arbitrary {
      def genNonNil(sz: Int): Gen[List[T]] = for {
        x  <- arbitrary[T]
        xs <- sizedList(sz/2)
      } yield Cons(x, xs)

      def sizedList(sz: Int) =
        if (sz <= 0) genNil
        else Gen.frequency((1, genNil), (3, genNonNil(sz)))

      Gen.sized(sz => sizedList(sz))
    }

  "List.tail(Cons(x, xs))" should "be xs" in forAll {
    (x: Int, xs: List[Int]) => List.tail(Cons(x, xs)) mustBe xs
  }

  "List.setHead(Cons(x, xs), h)" should "be Cons(h, xs)" in forAll {
    (x: Int, h: Int, xs: List[Int]) => List.setHead(Cons(x, xs), h) mustBe Cons(h, xs)
  }

  "List.init(xs ++ x)" should "be xs" in forAll {
    (x: Int, xs: List[Int]) => List.init(List.append(xs, Cons(x, Nil))) mustBe xs
  }

  "List.length(xs ++ ys)" should "be List.length(xs) + List.length(ys)" in forAll {
    (xs: List[Int], ys: List[Int]) => List.length(List.append(xs, ys)) mustBe List.length(xs) + List.length(ys)
  }

  "List.sumFoldLeft" should "be equal to List.sum" in forAll {
    (xs: List[Int]) => List.sumFoldLeft(xs) mustBe List.sum(xs)
  }

  /* FIXME: it's dead
  "List.productFoldLeft" should "be equal to List.product" in forAll {
    (xs: List[Double]) => List.productFoldLeft(xs) mustBe (List.product(xs) +- math.ulp(a))
  }
  */

  "List.appendFoldRight" should "be equal to List.append" in forAll {
    (xs: List[Int], ys: List[Int]) => List.appendFoldRight(xs, ys) mustBe List.append(xs, ys)
  }

  "List.reverse" should "be" in forAll {
    xs: List[Int] => List.reverse(List.reverse(xs)) mustBe xs
  }

  "List.flatten" should "be" in forAll {
    (xs: List[Int], ys: List[Int]) => List.flatten(List(xs, ys)) mustBe List.append(xs, ys)
  }

  "List.map" should "be" in forAll {
    xs: List[Int] => List.sum(List.map(xs)(_ + 1)) mustBe List.sum(xs) + List.length(xs)
  }

  val idOdd = (x: Int) => x % 2 == 0

  "List.filter" should "be" in forAll {
    xs: List[Int] => List.foldLeft(List.filter(xs)(idOdd), true)(_ && idOdd(_)) mustBe true
  }

  "List.flatMap" should "be" in forAll {
    xs: List[Int] => List.sum(List.flatMap(xs)(x => List(x, x))) mustBe List.sum(xs) * 2
  }

  "List.filterFlatMap" should "be" in forAll {
    xs: List[Int] => List.foldLeft(List.filterFlatMap(xs)(idOdd), true)(_ && idOdd(_)) mustBe true
  }

  def applyN[A](f: A => A, n: Int): A => A = {
    @tailrec
    def iter(g: A => A, i: Int): A => A =
      if (i == 1) g
      else iter(g.compose(f), i - 1)
    iter(f, n)
  }

  "applyN" should "be" in forAll (Gen.choose(1, 100)) {
    x => applyN[Int](_ + 1, x)(0) mustBe x
  }

  "List.+" should "be" in forAll {
    xs: List[Int] => {
      val i = Gen.choose(1, 100).sample.get
      applyN[List[Int]](List.+(xs, _), i)(xs) mustBe List.map(xs)(_ * (i + 1))
    }
  }

  "List.zipWith" should "be" in forAll {
    xs: List[Int] => {
      val i = Gen.choose(1, 100).sample.get
      applyN[List[Int]](List.zipWith(xs, _)(_ + _), i)(xs) mustBe List.map(xs)(_ * (i + 1))
    }
  }

  "List.hasSubsequence" should "be" in forAll {
    (xs: List[Int], ys: List[Int], zs: List[Int]) => {
      val xyzs = List.append(xs, List.append(ys, zs))
      List.hasSubsequence(xyzs, xs) mustBe true
      List.hasSubsequence(xyzs, ys) mustBe true
      List.hasSubsequence(xyzs, zs) mustBe true
    }
  }
}
