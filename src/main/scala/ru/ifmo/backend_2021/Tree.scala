package ru.ifmo.backend_2021

sealed trait Tree[A]

case class Leaf[A]() extends Tree[A]

case class Node[A](list: NonEmpty[A], left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def isEmpty: Tree[Int] => Boolean = {
    case _: Leaf[Int] => true
    case _ => false
  }

  def contains(value: Int): Tree[Int] => Boolean = {
    (tree: Tree[Int]) => {
      findNode(value, tree) match {
        case None => false
        case _ => true
      }
    }
  }

  def insert(value: Int): Tree[Int] => Tree[Int] = {
    (tree: Tree[Int]) => {
      tree match {
        case node: Node[Int] =>
          if (node.list.head == value)
            Node(node.list.copy(list = node.list.list :+ value), node.left, node.right)
          else if (node.list.head < value) {
            Node(list = node.list, node.left, insert(value)(node.right))
          }
          else {
            Node(list = node.list, insert(value)(node.left), node.right)
          }
        case _ =>
          Node(NonEmpty(value, List()), Leaf[Int](), Leaf[Int]())
      }
    }
  }

  def delete(value: Int): Tree[Int] => Tree[Int] = {
    (tree: Tree[Int]) => {
      tree match {
        case Leaf() => tree
        case Node(list, left, right) =>
          if (list.head == value) {
            if (list.list.nonEmpty) {
              Node(list.copy(list = list.list.slice(0, list.list.length - 1)), left, right)
            } else {
              left match {
                case _: Leaf[Int] if right.isInstanceOf[Leaf[Int]] =>
                  Leaf[Int]()
                case _: Leaf[Int] =>
                  right
                case _ => if (right.isInstanceOf[Leaf[Int]]) {
                  left
                } else {
                  val min = minNode(right.asInstanceOf[Node[Int]])
                  Node(min.list, left, delete(min.list.head)(right))
                }
              }
            }
          } else {
            if (value < list.head) {
              delete(value)(left)
            } else {
              delete(value)(right)
            }
          }
      }
    }
  }

  def fromList: List[Int] => Tree[Int] = {
    (list: List[Int]) => {
      if (list.isEmpty) {
        Leaf[Int]()
      } else {
        val sortedList = list.sorted
        val mid = (list.length - 1) / 2
        val midCount = sortedList.count(_ == sortedList(mid))
        val midEl = sortedList(mid)
        Node(
          NonEmpty(sortedList(mid), List.fill(midCount - 1)(midEl)),
          fromList(sortedList.slice(0, mid).filterNot(_ == midEl)),
          fromList(sortedList.slice(mid + 1, list.length).filterNot(_ == midEl)))
      }
    }
  }

  @scala.annotation.tailrec
  private def minNode(root: Node[Int]): Node[Int] = root.left match {
    case _: Leaf[Int] => root
    case _ => minNode(root.left.asInstanceOf[Node[Int]])
  }

  @scala.annotation.tailrec
  private def findNode(value: Int, tree: Tree[Int]): Option[Node[Int]] = {
    if (tree.isInstanceOf[Leaf[Int]]) {
      None
    } else {
      val node = tree.asInstanceOf[Node[Int]]
      if (node.list.head == value) {
        Option(node)
      } else {
        val nextTree = if (node.right.isInstanceOf[Leaf[Int]] ||
          node.right.asInstanceOf[Node[Int]].list.head > value) node.left else node.right
        findNode(value, nextTree)
      }
    }
  }

  lazy val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = fa match {
      case Leaf() => F.zero
      case Node(list, left, right) => F.op(F.op(NonEmpty.foldable.fold(list)(F), fold(left)(F)), fold(right)(F))
    }

    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf() => F.zero
      case Node(list, left, right) => F.op(foldMap(left)(f), F.op(NonEmpty.foldable.foldMap(list)(f), foldMap(right)(f)))
    }

    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = fa match {
      case Leaf() => z
      case Node(list, left, right) =>
        foldr(left, NonEmpty.foldable.foldr(list, foldr(right, z)(f))(f))(f)
    }
  }
}
