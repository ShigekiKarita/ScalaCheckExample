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
}
