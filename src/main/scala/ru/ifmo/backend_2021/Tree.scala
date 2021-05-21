package ru.ifmo.backend_2021

sealed trait Tree[A]
case class Leaf[A]() extends Tree[A]
case class Node[A](list: NonEmpty[A], left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def isEmpty: Tree[Int] => Boolean = {
    case Leaf() => true
    case Node(_, _, _) => false
  }

  def contains(value: Int): Tree[Int] => Boolean = {
    case _: Leaf[Int] => false
    case Node(NonEmpty(head, _), left, right) => head compare value match {
      case 0 => true
      case 1 => contains(value)(left)
      case -1 => contains(value)(right)
    }
  }

  def insert(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Node(NonEmpty(value, Nil), Leaf(), Leaf())
    case Node(NonEmpty(head, tail), left, right) => head compare value match {
      case 0 => Node(NonEmpty(head, value :: tail), left, right)
      case 1 => Node(NonEmpty(head, tail), insert(value)(left), right)
      case -1 => Node(NonEmpty(head, tail), left, insert(value)(right))
    }
  }

  def delete(value: Int): Tree[Int] => Tree[Int] = {
    case Leaf() => Leaf()
    case Node(NonEmpty(head, tail), left, right) => head compare value match {
      case 1 => Node(NonEmpty(head, tail), delete(value)(left), right)
      case -1 => Node(NonEmpty(head, tail), left, delete(value)(right))
      case _ => tail match {
        case _::xt => Node(NonEmpty(head, xt), left, right)
        case Nil =>
          def refresh: Tree[Int] => Tree[Int] = {
            case Node(c, l, r) => Node(c, l, refresh(r))
            case Leaf() => right
          }

          refresh(left)
      }
    }
  }

  def fromList(l: List[Int]): Tree[Int] = {
    val tr: Tree[Int] = Leaf()
    l.foldLeft(tr)((tree: Tree[Int], value: Int) => insert(value)(tree))
  }

  lazy val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = fa match {
      case Leaf() => F.zero
      case Node(list, left, right) => F.op(fold(left), F.op(NonEmpty.foldable.fold(list),fold(right)))
    }

    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf() => F.zero
      case Node(list, left, right) => F.op(foldMap(left)(f)(F), F.op(
        NonEmpty.foldable.foldMap(list)(f)(F),
        foldMap(right)(f)(F)
      )
      )
    }

    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = fa match {
      case Leaf() => z
      case Node(list, left, right) => foldr(
        left,
        NonEmpty.foldable.foldr(list, foldr(right, z)(f))(f))(f)
    }
  }

}
