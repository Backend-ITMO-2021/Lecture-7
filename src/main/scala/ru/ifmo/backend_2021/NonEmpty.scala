package ru.ifmo.backend_2021

case class NonEmpty[A](head: A, list: List[A])

object NonEmpty {
  val foldable: Foldable[NonEmpty] = new Foldable[NonEmpty] {
    def fold[A](fa: NonEmpty[A])(implicit F: Monoid[A]): A =
      foldMap(fa)(identity)
    def foldMap[A, B](fa: NonEmpty[A])(f: A => B)(implicit F: Monoid[B]): B =
      foldr(fa, F.zero)(a => b => F.op(f(a), b))
    def foldr[A, B](fa: NonEmpty[A], z: B)(f: A => B => B): B =
      f(fa.head)(fa.list.foldRight(z)((a, b) => f(a)(b)))
  }

  val functor: Functor[NonEmpty] = ???
  val applicative: Applicative[NonEmpty] = ???
  val monad: Monad[NonEmpty] = ???
}
