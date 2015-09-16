package errorhandling

import testutils.ScalaTestCommon


class OptionTest extends ScalaTestCommon {

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
    (a: Int) => {
      Some(a).getOrElse(() => 0) mustBe a
      none.getOrElse(a)          mustBe a
    }
  }

  "Some(a).orElse(b)" should "be Some(a) unless it's None else Some(b)" in forAll {
    (a: Int) => {
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
}
