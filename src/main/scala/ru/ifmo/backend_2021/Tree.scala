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
    case Node(NonEmpty(head, tail), left, right) => head match {
      case _ if value == head => true
      case _ if value < head => contains(value)(left)
      case _ => contains(value)(right)
    }
  }

  def insert(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Node(NonEmpty(value, Nil), Leaf(), Leaf())
    case Node(NonEmpty(head, tail), left, right) => head match {
      case _ if value == head => Node(NonEmpty(head, value :: tail), left, right)
      case _ if value < head => Node(NonEmpty(head, tail), insert(value)(left), right)
      case _ => Node(NonEmpty(head, tail), left, insert(value)(right))
    }
  }

  def delete(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Leaf()
    case Node(NonEmpty(head, tail), left, right) => head match {
      case _ if value < head => Node(NonEmpty(head, tail), delete(value)(left), right)
      case _ if value > head => Node(NonEmpty(head, tail), left, delete(value)(right))
      case _ => tail match {
        case x :: xs => Node(NonEmpty(head, xs), left, right)
        case _ => {
          def updateTree: Tree[Int] => Tree[Int] = {
            case Leaf() => right
            case Node(v, l, r) => Node(v, l, updateTree(r))
          }
          updateTree(left)
        }
      }
    }
  }
  
  def fromList: List[Int] => Tree[Int] =
    _.foldLeft(Leaf(): Tree[Int])((tree: Tree[Int], value: Int) => insert(value)(tree))

  lazy val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = fa match {
      case Node(list, left, right) => F.op(
        fold(left)(F),
        F.op(
          NonEmpty.foldable.fold(list)(F),
          fold(right)(F)
        )
      )
      case Leaf() => F.zero
    }

    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Node(list, left, right) => F.op(
        foldMap(left)(f)(F),
        F.op(
          NonEmpty.foldable.foldMap(list)(f)(F),
          foldMap(right)(f)(F)
        )
      )
      case Leaf() => F.zero
    }

    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = fa match {
      case Node(list, left, right) => foldr(
        left,
        NonEmpty.foldable.foldr(
          list,
          foldr(right, z)(f)
        )(f)
      )(f)
      case Leaf() => z
    }
  }
}
