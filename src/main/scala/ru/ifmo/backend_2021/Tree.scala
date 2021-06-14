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
    case Leaf() => false
    case Node(v, left, right) => {
      if (value == v.head) true
      else contains(value)(if (value < v.head) left else right)
    }
  }

  def insert(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Node(NonEmpty(value, Nil), Leaf(), Leaf())
    case Node(v, left, right) => {
      if (value == v.head) Node(NonEmpty(v.head, value :: v.list), left, right)
      else if (value < v.head) Node(v, insert(value)(left), right)
      else Node(v, left, insert(value)(right))
    }
  }

  def delete(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Leaf()
    case Node(NonEmpty(head, list), left, right) => {
      if (value == head) list match {
        case _ :: lt => Node(NonEmpty(head, lt), left, right)
        case Nil => {
          def updateTree: Tree[Int] => Tree[Int] = {
            case Node(v, left, right) => Node(v, left, updateTree(right))
            case Leaf() => right
          }

          updateTree(left)
        }
      }
      else if (value < head) Node(NonEmpty(head, list), delete(value)(left), right)
      else Node(NonEmpty(head, list), left, delete(value)(right))
    }
  }

  def fromList: List[Int] => Tree[Int] = _.foldLeft(Leaf(): Tree[Int]) {
    (tree: Tree[Int], value: Int) => insert(value)(tree)
  }

  lazy val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = fa match {
      case Leaf() => F.zero
      case Node(value, left, right) => F.op(fold(left), F.op(NonEmpty.foldable.fold(value),fold(right)))
    }

    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf() => F.zero
      case Node(value, left, right) => F.op(foldMap(left)(f), F.op(NonEmpty.foldable.foldMap(value)(f),foldMap(right)(f)))
    }

    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = fa match {
      case Leaf() => z
      case Node(value, left, right) => foldr(left, NonEmpty.foldable.foldr(value, foldr(right, z)(f))(f))(f)
    }
  }
}
