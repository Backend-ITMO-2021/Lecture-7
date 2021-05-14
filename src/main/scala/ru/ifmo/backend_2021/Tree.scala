package ru.ifmo.backend_2021

sealed trait Tree[A]

case class Leaf[A]() extends Tree[A]

case class Node[A](list: NonEmpty[A], left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def isEmpty: Tree[Int] => Boolean = {
    case Leaf() => true
    case _ => false
  }

  def contains(value: Int): Tree[Int] => Boolean = {
    case Leaf() => false;
    case Node(NonEmpty(head, _), _, _) if value == head => true
    case Node(NonEmpty(head, _), left, _) if value < head => contains(value)(left)
    case Node(_, _, right) => contains(value)(right)
  }

  def insert(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Node(NonEmpty.applicative.point(value), Leaf(), Leaf())
    case node@Node(NonEmpty(head, seq), _, _) if value == head => node.copy(list = NonEmpty(head, value +: seq))
    case node@Node(NonEmpty(head, _), left, _) if value < head => node.copy(left = insert(value)(left))
    case node@Node(NonEmpty(head, _), _, right) if value > head => node.copy(right = insert(value)(right))
  }

  def findMin: Tree[Int] => NonEmpty[Int] = {
    case Node(ne, Leaf(), _) => ne
    case Node(_, left, _) => findMin(left)
  }

  def delete(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Leaf()

    case node@Node(NonEmpty(head, _), left, _) if value < head => node.copy(left = delete(value)(left))

    case node@Node(NonEmpty(head, _), _, right) if value > head => node.copy(right = delete(value)(right))

    case Node(NonEmpty(head, Nil), Leaf(), right) if value == head => right

    case Node(NonEmpty(head, Nil), left, Leaf()) if value == head => left

    case node@Node(NonEmpty(head, Nil), left, right) if value == head =>
      val newNonEmpty = findMin(right)
      node.copy(list = newNonEmpty.copy(), left, delete(newNonEmpty.head)(right))

    case node@Node(NonEmpty(head, _ +: seq), _, _) if value == head => node.copy(list = NonEmpty(head, seq))
  }

  def fromList: List[Int] => Tree[Int] = _.foldLeft[Tree[Int]](Leaf()) { case (tree, value) => insert(value)(tree) }

  lazy val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = fa match {
      case Leaf() => F.zero
      case Node(nonEmpty, left, right) => F.op(F.op(fold(left), NonEmpty.foldable.fold(nonEmpty)), fold(right))
    }

    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf() => F.zero
      case Node(nonEmpty, left, right) => F.op(F.op(foldMap(left)(f), NonEmpty.foldable.foldMap(nonEmpty)(f)), foldMap(right)(f))
    }

    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = fa match {
      case Leaf() => z
      case Node(nonEmpty, left, right) => foldr(left, f(nonEmpty.head)(foldr(right, z)(f)))(f)
    }
  }
}
