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
    override def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] =
      NonEmpty(f(fa.head), fa.list.map(a => f(a)))
  }
  lazy val applicative: Applicative[NonEmpty] = new Applicative[NonEmpty] {
    override def point[A](a: A): NonEmpty[A] = NonEmpty(a, List())

    override def ap[A, B](fa: NonEmpty[A])(f: NonEmpty[A => B]): NonEmpty[B] = {
      val list = for {fs <- f.head :: f.list; fas <- fa.head :: fa.list} yield fs(fas)
      NonEmpty(list.head, list.tail)
    }

    override def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = functor.map(fa)(f)
  }
  lazy val monad: Monad[NonEmpty] = new Monad[NonEmpty] {
    override def point[A](a: A): NonEmpty[A] = NonEmpty(a, List())

    override def flatMap[A, B](fa: NonEmpty[A])(f: A => NonEmpty[B]): NonEmpty[B] = {
      val list = for {a <- fa.head :: fa.list; b <- f(a).head :: f(a).list} yield b
      NonEmpty(list.head, list.slice(1, list.length))
    }
  }
}
