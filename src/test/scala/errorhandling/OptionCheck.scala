package errorhandling

import org.scalacheck.Properties
import org.scalacheck.Prop._

object OptionCheck extends Properties("Option[Int]") {
  val none: Option[Int] = None

  property("get Some") = forAll {
    (a: Int) => Some(a).get == a
  }

  property("get None") = forAll {
    (a: Int) => Some(none).get == None
  }

  property("map Some") = forAll {
    (a: Int, b: Int) => Some(a).map(_ * b) == Some(a * b)
  }

  property("map None") = forAll {
    (a: Int) => none.map(_ * a) == None
  }

  property("flatMap Some") = forAll {
    (a: Int, b: Int) => Some(a).flatMap(x => Some(x * b)) == Some(a * b)
  }

  property("flatMap None") = forAll {
    (a: Int) => none.flatMap(x => Some(x * a)) == None
  }

  property("getOrElse Some") = forAll {
    (a: Int) => Some(a).getOrElse(() => 0) == a
  }

  property("getOrElse None") = forAll {
    (a: Int) => none.getOrElse(a) == a
  }

  property("orElse Some") = forAll {
    (a: Int) => Some(a).orElse[Int](none) == Some(a)
  }

  property("orElse None") = forAll {
    (a: Int) => none.orElse(Some(a)) == Some(a)
  }
}
