package testutils

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, FlatSpec}


class ScalaTestCommon extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(
    workers = 8,
    minSuccessful = 100000
  )
}

