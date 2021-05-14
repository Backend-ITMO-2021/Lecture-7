package ru.ifmo.backend_2021

case class NonEmpty[A](head: A, list: List[A])

object NonEmpty {
  lazy val foldable: Foldable[NonEmpty] = new Foldable[NonEmpty] {
    def fold[A](fa: NonEmpty[A])(implicit F: Monoid[A]): A =
      foldMap(fa)(identity)
    def foldMap[A, B](fa: NonEmpty[A])(f: A => B)(implicit F: Monoid[B]): B =
      foldr(fa, F.zero)(a => b => F.op(f(a), b))
    def foldr[A, B](fa: NonEmpty[A], z: B)(f: A => B => B): B =
      f(fa.head)(fa.list.foldRight(z)((a, b) => f(a)(b)))
  }

  lazy val functor: Functor[NonEmpty] = new Functor[NonEmpty] {
    def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = NonEmpty(f(fa.head), fa.list.map(f))
  }
  lazy val applicative: Applicative[NonEmpty] = new Applicative[NonEmpty] {
    def point[A](a: A): NonEmpty[A] = NonEmpty(a, Nil)

    def ap[A, B](fa: NonEmpty[A])(f: NonEmpty[A => B]): NonEmpty[B] = f match {
      case NonEmpty(head, first :: tail) =>
        val newHead = map(fa)(head)
        val newList = ap(fa)(NonEmpty(first, tail))
        NonEmpty(newHead.head, newHead.list ++ (newList.head :: newList.list))
      case NonEmpty(head, Nil) => map(fa)(head)
    }

    def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = NonEmpty.functor.map(fa)(f)
  }
  lazy val monad: Monad[NonEmpty] = new Monad[NonEmpty] {
    def point[A](a: A): NonEmpty[A] = NonEmpty.applicative.point(a)

    def flatMap[A, B](fa: NonEmpty[A])(f: A => NonEmpty[B]): NonEmpty[B] = fa match {
      case NonEmpty(head, first :: tail) =>
        val newHead = f(head)
        val newList = flatMap(NonEmpty(first, tail))(f)
        NonEmpty(newHead.head, newHead.list ++ (newList.head :: newList.list))
      case NonEmpty(head, Nil) => f(head)
    }
  }
}
