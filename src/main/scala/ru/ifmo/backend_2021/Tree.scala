package ru.ifmo.backend_2021

sealed trait Tree[A]

object Tree {
  def isEmpty[A]: Tree[A] => Boolean = ???
  def contains[A]: A => Tree[A] =>Boolean = ???
  def insert[A]: A => Tree[A] => Tree[A] = ???
  def delete[A]: A => Tree[A] => Tree[A] = ???
  def fromList[A]: List[A] => Tree[A] = ???

  val foldable = new Foldable[Tree] {
    def fold[A](fa: Tree[A])(implicit F: Monoid[A]): A = ???
    def foldMap[A, B](fa: Tree[A])(f: A => B)(implicit F: Monoid[B]): B = ???
    def foldr[A, B](fa: Tree[A], z: B)(f: A => B => B): B = ???
  }

  val list = List(1,2,3)
  foldable.toList(fromList(list)) == list.sorted
}
