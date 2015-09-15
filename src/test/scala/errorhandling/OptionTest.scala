package errorhandling

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, FlatSpec}

class OptionTest extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(workers = 8, minSuccessful = 200)
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
