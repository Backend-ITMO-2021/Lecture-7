package ru.ifmo.backend_2021

sealed trait Tree[A]
case class Leaf[A]() extends Tree[A]
case class Node[A](list: NonEmpty[A], left: Tree[A], right: Tree[A])

object Tree {
  def isEmpty: Tree[Int] => Boolean = ???
  def contains(value: Int): Tree[Int] => Boolean = ???
  def insert(value: Int): Tree[Int] => Tree[Int] = ???
  def delete(value: Int): Tree[Int] => Tree[Int] = ???
  def fromList: List[Int] => Tree[Int] = ???

  val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = ???
    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = ???
    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = ???
  }
}
