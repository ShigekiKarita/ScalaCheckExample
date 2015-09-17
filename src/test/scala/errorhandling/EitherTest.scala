package errorhandling

import test.ScalaTestCommon


class EitherTest extends ScalaTestCommon {

  "Either.mean" should "be" in {
    Either.mean(IndexedSeq()) mustBe Left("mean of empty List!")
  }
}
