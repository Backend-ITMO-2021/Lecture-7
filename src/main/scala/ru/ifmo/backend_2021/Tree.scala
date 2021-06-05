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
    case tree if isEmpty(tree) => false
    case Node(NonEmpty(head, _), left, right) =>
      if (value == head)
        true
      else if (value > head)
        contains(value)(right)
      else
        contains(value)(left)
  }
  def insert(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Node(NonEmpty(value, Nil), Leaf(), Leaf())
    case Node(NonEmpty(head, list), left, right) =>
      if (value == head)
        Node(NonEmpty(head, value :: list), left, right)
      else if (value < head)
        Node(NonEmpty(head, list), insert(value)(left), right)
      else
        Node(NonEmpty(head, list), left, insert(value)(right))
  }
  def delete(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Leaf()
    case Node(NonEmpty(head, list), left, right) =>
      if (value < head)
        Node(NonEmpty(head, list), delete(value)(left), right)
      else if (value > head)
        Node(NonEmpty(head, list), left, delete(value)(right))
      else {
        if (list.nonEmpty) {
          Node(NonEmpty(head, list.drop(1)), left, right)
        }
        else {
          fromList(getValuesFromTree(left).++(getValuesFromTree(right)))
        }
      }
  }
  def fromList(list: List[Int]): Tree[Int] = {
    var tree: Tree[Int] = Leaf()
    list.foreach {
      value =>
        tree = insert(value)(tree)
    }
    tree
  }
  def getValuesFromTree: Tree[Int] => List[Int] = {
    case Leaf() => List.empty
    case Node(NonEmpty(head, list), left, right) => head :: list.++(getValuesFromTree(left).++(getValuesFromTree(right)))
  }

  lazy val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A =
      fa match {
        case Leaf() => F.zero
        case t => foldMap(t)(x => x)
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