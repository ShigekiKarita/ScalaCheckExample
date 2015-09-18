package part1.errorhandling

import org.scalacheck.{Gen, Arbitrary}
import common.ScalaTestCommon


class EitherTest extends ScalaTestCommon {
  val left: Either[String, Any] = Left("error")

  implicit def arbEither[T](implicit a: Arbitrary[T]): Arbitrary[Either[String, T]] =
    Arbitrary {
      val genLeft = Gen.const(Left("error"))
      val genRight = for { x <- Arbitrary.arbitrary[T] } yield Right(x)
      Gen.frequency((1, genLeft), (9, genRight))
    }

  "Either.mean and Either.getOrElse" should "be" in forAll {
    ds: IndexedSeq[Double] =>{
      val xs = ds.map(_ / Double.MaxValue) // to avoid the numeric-flow
      val result = Either.mean(xs).getOrElse(0.0) * xs.length
      result mustBe (xs.sum +- math.ulp(result) * eps)
    }
  }

  "Right(a).map(f)" should "be" in forAll {
    (a: Int, b: Double) => Right(a).map(_ => b) mustBe Right(b)
  }

  "Left(a).map(f)" should "be" in forAll {
    (a: Int, b: Double) => Left(a).map(_ => b) mustBe Left(a)
  }

  "Right(a).flatMap(f)" should "be" in forAll {
    (a: Int, b: Double) => Right(a).flatMap(_ => Right(b)) mustBe Right(b)
  }

  "Left(a).flatMap(f)" should "be" in forAll {
    (a: Int, b: Double) => Left(a).flatMap(_ => Right(b)) mustBe Left(a)
  }

  "Either.orElse" should "be" in forAll {
    a: Int => {
      Right(a).orElse(left) mustBe Right(a)
      left.orElse(Right(a)) mustBe Right(a)
    }
  }

  "Either.sequence and Either.map2" should "be" in forAll {
    xs: List[Either[String, Int]] => {
      val result = Either.sequence(xs)
      result.map(_.sum) mustBe xs.foldLeft[Either[String, Int]](Right(0))(_.map2(_)(_ + _))
      if (result != left) {
        result.getOrElse(List()) mustBe xs.map(_.getOrElse(0))
      }
    }
  }

  "Person.mkPerson" should "be" in {
    Person.mkPerson("", 12) mustBe Left(List("Name is empty."))
    Person.mkPerson("John", -2) mustBe Left(List("Age is invalid."))
    Person.mkPerson("", -2) mustBe Left(List("Name is empty.", "Age is invalid."))
    val result = Person.mkPerson("John", 12).getOrElse(new Person(new Name(""), new Age(-1)))
    result.name.value mustBe "John"
    result.age.value mustBe 12
  }
}
