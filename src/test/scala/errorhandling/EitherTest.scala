package errorhandling

import test.ScalaTestCommon


class EitherTest extends ScalaTestCommon {

  "Either.mean" should "be" in forAll {
    xs: IndexedSeq[Double] => Either.mean(xs).leftSideValue
  }
}
