package common

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, FlatSpec}


class ScalaTestCommon extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(
    workers = 8,
    minSuccessful = 10000
  )
  val eps = 1.0E2
  val lengthGen = Gen.choose(0, 100)
  val doubleGen = Gen.choose(-1.0, 1.0)
  val intGen = Gen.choose(Int.MinValue, Int.MaxValue)
}