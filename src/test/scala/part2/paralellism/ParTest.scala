package part2.paralellism

import java.util.concurrent.Executors

import common.ScalaTestCommon


class ParTest extends ScalaTestCommon {
  def go[A](pa: Par.Par[A]): A =
    Par.run(Executors.newCachedThreadPool())(pa).get()

  "Par.sum" should "be" in forAll {
    xs: IndexedSeq[Int] => go(Par.sum(xs)) mustBe xs.sum
  }

  "Par.perSort" should "be" in forAll {
    xs: IndexedSeq[Int] => go(Par.parSort(Par.unit(xs))) mustBe xs.sorted
  }

  "Par.parMap" should "be" in forAll {
    (xs: List[Int], i: Int) => go(Par.parMap(xs)(_ + i)) mustBe xs.map(_ + i)
  }

  "Par.parFilter" should "be" in forAll {
    (xs: List[Int], i: Int) => go(Par.parFilter(xs)(_ < i)) mustBe xs.filter(_ < i)
  }

  "Par.equal" should "be" in forAll {
    x: Int => Par.equal(Executors.newSingleThreadExecutor())(
      Par.map(Par.unit(x))(_ + 1), Par.unit(x + 1)) mustBe true
  }
}
