package part1.state

import common.ScalaTestCommon
import org.scalacheck.Gen

class StateTest extends ScalaTestCommon {
  "RNG.SimpleRNG" should "be" in {
    val rng0 = RNG.SimpleRNG(42)
    val (n1, rng1) = rng0.nextInt
    n1 mustBe 16159453
    val (n2, rng2) = RNG.int(rng1)
    n2 mustBe -1281479697
  }

  "RNG.unit" should "be" in forAll {
    (x: Int, y: Int) => {
      val rng = RNG.SimpleRNG(x)
      val (n, r) = RNG.unit(y)(rng)
      n mustBe y
      r mustBe rng
    }
  }

  "RNG.nonNegativeInt" should "be" in forAll {
    x: Int => RNG.nonNegativeInt(RNG.SimpleRNG(x))._1 must be >= 0
  }

  "RNG.double" should "be" in forAll {
    x: Int => {
      val result = RNG.double(RNG.SimpleRNG(x))._1
      result must be >= 0.0
      result must be < 1.0
    }
  }

  "RNG.nonNegativeEven" should "be" in forAll {
    x: Int => RNG.nonNegativeEven(RNG.SimpleRNG(x))._1 % 2 == 0 mustBe true
  }

  "RNG.both" should "be" in forAll {
    x: Int => {
      val a = RNG.SimpleRNG(x)
      val d = RNG.double(a)
      val i = RNG.int(d._2)
      val result = RNG.both(RNG.double, RNG.int)(a)
      result._1 mustBe (d._1, i._1)
    }
  }

  "RNG.nonNegativeLessThan" should "be" in forAll (Gen.choose(1, Int.MaxValue), intGen) {
    (x: Int, y: Int) =>
      val result = RNG.nonNegativeLessThan(x)(RNG.SimpleRNG(y))._1
      result must be < x
      result must be >= 0
  }

  "RNG.ints" should "be" in forAll (lengthGen, intGen) {
    (x: Int, y: Int) => RNG.ints(x)(RNG.SimpleRNG(x))._1.length mustBe x
  }

  "State" should "be" in forAll {
    (x: Int, y: Int) => {
      val rng = RNG.SimpleRNG(x)
      State.unit(y).run(rng) mustBe RNG.unit(y)(rng)
    }
  }

  "Machine.simulate" should "be" in {
    val r0 = new Machine(false, 5, 10)
    val r1 = Machine.simulate(List(Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin)).run(r0)
    r1._1 mustBe (14, 1)
    val r2 = Machine.simulate(List(Turn, Turn)).run(r1._2)
    r2._1 mustBe (14, 0)
    val r3 = new Machine(false, 1, 0)
    val r4 = Machine.simulate(List(Coin, Coin)).run(r3)
    r4._2 mustBe r3
    val r5 = new Machine(true, 1, 0)
    val r6 = Machine.simulate(List(Turn, Turn)).run(r5)
    r6._2 mustBe r5
  }
}
