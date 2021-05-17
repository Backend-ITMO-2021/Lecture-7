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
      else if (value < v.head) contains(value)(left)
      else contains(value)(right)
    }
  }

  def insert(value: Int): Tree[Int] => Tree[Int] =  {
    case Leaf() => Node(NonEmpty(value, Nil), Leaf(), Leaf())
    case Node(v, left, right) => {
      if (value == v.head)  Node(NonEmpty(v.head, value :: v.list), left, right)
      else if (value < v.head) Node(v, insert(value)(left), right)
      else Node(v, left, insert(value)(right))
    }
  }

  def delete(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Leaf()
    case Node(v, left, right) => {
      if (value == v.head) v.list match {
        case head :: tail => Node(NonEmpty(head, tail), left, right)
        case Nil => {
          def update(tree: Tree[Int]): Tree[Int] = tree match {
            case Node(v, left, right) => Node(v, left, update(right))
            case Leaf() => right
          }

          update(left)
        }
      }
      else if (value < v.head) Node(v, delete(value)(left), right)
      else Node(v, left, delete(value)(right))
    }
  }

  def fromList(list: List[Int]): Tree[Int] = list.foldLeft(Leaf(): Tree[Int])((tree: Tree[Int], value: Int) => insert(value)(tree))

  lazy val foldable: Foldable[Tree] = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = fa match {
      case Leaf() => F.zero
      case Node(list,left,right) => F.op(fold(left), F.op(NonEmpty.foldable.fold(list),fold(right)))
    }
    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf() => F.zero
      case Node(list,left,right) => F.op(foldMap(left)(f), F.op(NonEmpty.foldable.foldMap(list)(f),foldMap(right)(f)))
    }
    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = fa match {
      case Leaf() => z
      case Node(list,left,right) => foldr(left, f(list.head)(foldr(right, z)(f)))(f)
    }
  }
}
