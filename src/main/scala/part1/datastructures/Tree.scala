package part1.datastructures

sealed trait Tree[+A] {
  def +[B>: A](that: Tree[B]): Tree[B] = this match {
    case Node(l, r) => Node(l, r + that)
    case Leaf(a) => Node(Leaf(a), that)
  }

  def size: Int = this match {
    case Node(l, r) => l.size + 1 + r.size
    case Leaf(_) => 1
  }

  def depth: Int = this match {
    case Node(l, r) => (l.depth max r.depth) + 1
    case Leaf(_) => 1
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Node(l, r) => Node(l.map(f), r.map(f))
    case Leaf(a) => Leaf(f(a))
  }

  def fold[B](nf: (Tree[A], Tree[A]) => B, lf: A => B): B = this match {
    case Node(l, r) => nf(l, r)
    case Leaf(a) => lf(a)
  }

  def sizeFold: Int = fold(_.size + 1 + _.size, _ => 1)

  def depthFold: Int = fold((l, r) => (l.depthFold max r.depthFold) + 1, _ => 1)

  def mapFold[B](f: A => B): Tree[B] = fold[Tree[B]](
    (l, r) => Node(l.mapFold(f), r.mapFold(f)),
    x => Leaf(f(x))
  )
}
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Node(l, r) => maximum(l) max maximum(r)
  }

  def maximumFold(t: Tree[Int]): Int = t.fold(maximum(_) max maximum(_), identity)
}