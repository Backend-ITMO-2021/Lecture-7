package ru.ifmo.backend_2021

case class NonEmpty[A](head: A, list: List[A]) {
  def toList: List[A] = head :: list
  def add(a: A): NonEmpty[A] = NonEmpty(a, toList)
}

object NonEmpty {
  val monad: Monad[NonEmpty] = new Monad[NonEmpty] {
    def point[A](a: A): NonEmpty[A] = NonEmpty(a, Nil)
    def flatMap[A, B](fa: NonEmpty[A])(f: A => NonEmpty[B]): NonEmpty[B] = {
      val NonEmpty(a, list) = fa
      val b = f(a)
      val bList = list.map(f)
      NonEmpty(b.head, b.list ::: bList.flatMap(_.toList))
    }
  }
  val functor: Functor[NonEmpty] = new Functor[NonEmpty] {
    def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = NonEmpty(f(fa.head), fa.list.map(f))
  }
  val foldable: Foldable[NonEmpty] = new Foldable[NonEmpty] {
    def fold[A](fa: NonEmpty[A])(implicit F: Monoid[A]): A =
      foldMap(fa)(identity)
    def foldMap[A, B](fa: NonEmpty[A])(f: A => B)(implicit F: Monoid[B]): B =
      foldr(fa, F.zero)(a => b => F.op(b, f(a)))
    def foldr[A, B](fa: NonEmpty[A], z: B)(f: A => B => B): B =
      f(fa.head)(fa.list.foldRight(z)((a, b) => f(a)(b)))
  }
}
