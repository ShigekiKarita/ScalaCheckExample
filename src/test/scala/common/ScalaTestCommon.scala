package common

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, FlatSpec}


class ScalaTestCommon extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(
    workers = 8,
    minSuccessful = 10000
  )
  val eps = 1.0E2
}