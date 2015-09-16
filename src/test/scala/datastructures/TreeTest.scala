package datastructures

import org.scalacheck.{Gen, Arbitrary}
import test.ScalaTestCommon

class TreeTest extends ScalaTestCommon {

  implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] =
    Arbitrary {
      val genLeaf = for(e <- Arbitrary.arbitrary[T]) yield Leaf(e)

      def genNode(sz: Int): Gen[Tree[T]] = for {
        l <- sizedTree(sz/2)
        r <- sizedTree(sz/2)
      } yield Node(l, r)

      def sizedTree(sz: Int) =
        if(sz <= 0) genLeaf
        else Gen.frequency((1, genLeaf), (3, genNode(sz)))

      Gen.sized(sz => sizedTree(sz))
    }

  "Tree.size" should "be merged well" in forAll {
    (x: Tree[Int], y: Tree[Int]) => (x + y).size mustBe x.size + y.size + 1
  }

  "Tree.maximum" should "be" in forAll {
    (x: Tree[Int], y: Tree[Int]) => Tree.maximum(x + Leaf(Int.MaxValue) + y) mustBe Int.MaxValue
  }

  "Tree.depth" should "be" in forAll {
    x: Tree[Int] => {
      x.depth must be <= x.size
      x.depth.toDouble must be > math.log(x.size)./(math.log(2))
    }
  }

  "Tree.map" should "be" in forAll {
    x: Tree[Double] => Tree.maximum(x.map(_ => Int.MinValue)) mustBe Int.MinValue
  }

  "Tree.fold children" should "be" in forAll {
    (x: Tree[Int], i: Int) => {
      x.sizeFold mustBe x.size
      Tree.maximumFold(x) mustBe Tree.maximum(x)
      x.depthFold mustBe x.depth
      x.mapFold(_ * i) mustBe x.map(_ * i)
    }
  }
}
