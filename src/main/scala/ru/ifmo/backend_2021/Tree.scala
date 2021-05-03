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
          else node.right match {
            case rightNode: Node[Int] =>
              if (rightNode.list.head > value) Node(node.list, insert(value)(node.left), node.right)
              else Node(node.list, node.left, insert(value)(node.right))
            case _ =>
              node.left match {
                case leftNode: Node[Int] =>
                  if (leftNode.list.head < value) Node(node.list, node.left, insert(value)(node.right))
                  else Node(node.list, insert(value)(node.left), node.right)
                case _ =>
                  if (value < node.list.head) Node(node.list, insert(value)(node.left), node.right)
                  else Node(node.list, node.left, insert(value)(node.right))
              }
          }
        case _ =>
          Node(NonEmpty(value, List(value)), Leaf[Int](), Leaf[Int]())
      }
    }
  }

  def delete(value: Int): Tree[Int] => Tree[Int] = {
    (tree: Tree[Int]) => {
      tree match {
        case Leaf() => tree
        case Node(list, left, right) =>
          if (list.head == value) {
            if (list.list.length > 1) {
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
        Node(
          NonEmpty(sortedList(mid), List(sortedList(mid))),
          fromList(sortedList.slice(0, mid)),
          fromList(sortedList.slice(mid + 1, list.length)))
      }
    }
  }

  @scala.annotation.tailrec
  private def minNode(root: Node[Int]): Node[Int] = {
    if (root.left.isInstanceOf[Leaf[Int]]) {
      root
    } else {
      minNode(root.left.asInstanceOf[Node[Int]])
    }
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
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = ???

    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = ???

    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = ???
  }
}
