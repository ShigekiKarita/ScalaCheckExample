package part1.errorhandling

import org.scalacheck.{Arbitrary, Gen}
import common.ScalaTestCommon


class OptionTest extends ScalaTestCommon {

  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] =
    Arbitrary {
      val genNone = Gen.const(None)
      val genSome = for { x <- Arbitrary.arbitrary[T] } yield Some(x)
      Gen.frequency((1, genNone), (9, genSome))
    }

  val none: Option[Int] = None

  "Some(a).get" should "be a" in forAll {
    (a: Int) => {
      Some(a).get    mustBe a
      Some(none).get mustBe None
    }
  }

  "Some(a).map(f)" should "be f(a): A unless it's None" in forAll {
    (a: Int, b: Int) => {
      Some(a).map(_ * b) mustBe Some(a * b)
      none.map(_ * b)    mustBe None
    }
  }

  "Some(a).flatMap(f)" should "be f(a): Option[A] unless it's None" in forAll {
    (a: Int, b: Int) => {
      Some(a).flatMap(x => Some(x * b)) mustBe Some(a * b)
      none.flatMap(x => Some(x * a))    mustBe None
    }
  }

  "Some(a).getOrElse(b)" should "be a unless it's None else b" in forAll {
    a: Int => {
      Some(a).getOrElse(() => 0) mustBe a
      none.getOrElse(a)          mustBe a
    }
  }

  "Some(a).orElse(b)" should "be Some(a) unless it's None else Some(b)" in forAll {
    a: Int => {
      Some(a).orElse[Int](none) mustBe Some(a)
      none.orElse(Some(a))      mustBe Some(a)
    }
  }

  "Option.filter" should "be" in forAll {
    a: Boolean => {
      val result = Some(a).filter(_ => a)
      if (a) result mustBe Some(a)
      else result mustBe None
    }
  }

  "Option.mean" should "be" in forAll {
    ds: List[Double] => {
      val xs = ds.map(_ / Double.MaxValue) // to avoid the numeric-flow
      val result = Option.mean(xs).getOrElse(0.0) * xs.length
      result mustBe (xs.sum +- math.ulp(result) * eps)
    }
  }

  "Option.variance" should "be" in forAll (Gen.listOfN(100, Gen.choose(-1.0, 1.0))) {
    xs: List[Double] => {
      val s = math.sqrt(Option.variance(xs).getOrElse(1.0))
      val m = Option.mean(xs).getOrElse(0.0)
      if (s != 0.0) {
        val result = Option.variance(xs.map(x => (x - m) / s)).getOrElse(1.0)
        result mustBe (1.0 +- math.ulp(s) * eps)
      }
    }
  }

  "Option.lift" should "be" in forAll {
    (x: Option[Int], i: Double) => Option.lift[Int, Double](i *)(x) mustBe x.map(i *)
  }

  "Option.sequence and Option.map2" should "be" in forAll {
    xs: List[Option[Int]] => {
      val result = Option.sequence(xs)
      result.map(_.sum) mustBe xs.foldLeft[Option[Int]](Some(0))(Option.map2(_, _)(_ + _))
      if (result != None) {
        result.getOrElse(List()) mustBe xs.map(_.getOrElse(0))
      }
    }
  }
  
  // Option usage
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def parseInts(a: List[String]): Option[List[Int]] =
    Option.sequence(a map (i => Try(i.toInt)))

  "Option.traverse" should "be" in forAll {
    is: List[Option[Int]] => {
      val xs = is
        .map(_.getOrElse(0))
        .map(_.toString)
        .map(x => if (x == "0") "None" else x)
      Option.traverse(xs)(x => Try(x.toInt)) mustBe parseInts(xs)
    }
  }
}
