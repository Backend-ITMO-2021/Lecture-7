package ru.ifmo.backend_2021

import scala.annotation.tailrec

sealed trait Tree[A]
case class Leaf[A]() extends Tree[A]
case class Node[A](list: NonEmpty[A], left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def isEmpty(t: Tree[Int]): Boolean = t match {
    case Node(_, _, _) => false
    case Leaf() => true
  }

  @tailrec
  def contains(value: Int)(t: Tree[Int]): Boolean = t match {
    case Node(c, l, r) => c.head == value || this.contains(value)(if (value < c.head) l else r)
    case Leaf() => false
  }

  def insert(value: Int)(t: Tree[Int]): Tree[Int] = t match {
    case Node(c, l, r) =>
      if (value == c.head) Node(NonEmpty(c.head, value :: c.list), l, r)
      else if (value < c.head) Node(c, insert(value)(l), r)
      else Node(c, l, insert(value)(r))
    case Leaf() => Node(NonEmpty(value, List.empty), Leaf(), Leaf())
  }

  def delete(value: Int)(t: Tree[Int]): Tree[Int] = t match {
    case Node(c, l, r) =>
      if (value == c.head) c.list match {
        case x :: xs => Node(NonEmpty(x, xs), l, r)
        case Nil =>
          def f(t: Tree[Int]): Tree[Int] = t match {
            case Node(c, l, r) => Node(c, l, f(r))
            case Leaf() => r
          }

          f(l)
      }
      else if (value < c.head) Node(c, delete(value)(l), r)
      else Node(c, l, delete(value)(r))
    case Leaf() => Leaf()
  }

  def fromList(l: List[Int]): Tree[Int] =
    l.foldLeft(Leaf().asInstanceOf[Tree[Int]]) { (acc: Tree[Int], x) => this.insert(x)(acc) }

  lazy val foldable = new Foldable[Tree] {

    def fold[A](t: Tree[A])(implicit F: Monoid[A]): A =
      this.foldr(t, F.zero) { a => b => F.op(a, b) }

    def foldMap[A, B](t: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = t match {
      case Node(c, l, r) => F.op(
        F.op(
          this.foldMap(l)(f)(F),
          NonEmpty.foldable.foldMap(c)(f)(F)
        ),
        this.foldMap(r)(f)(F)
      )
      case Leaf() => F.zero
    }

    def foldr[A, B](t: Tree[A], z: B)(f: A => B => B): B = t match {
      case Node(c, l, r) => this.foldr(l, NonEmpty.foldable.foldr(c, this.foldr(r, z)(f))(f))(f)
      case Leaf() => z
    }
  }
}
