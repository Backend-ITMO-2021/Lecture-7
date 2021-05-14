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
    override def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = NonEmpty(f(fa.head), fa.list.map(f))
  }
  lazy val applicative: Applicative[NonEmpty] = new Applicative[NonEmpty] {
    override def point[A](a: A): NonEmpty[A] = NonEmpty(a, List())

    override def ap[A, B](fa: NonEmpty[A])(f: NonEmpty[A => B]): NonEmpty[B] = f match {
      case NonEmpty(head, Nil) => NonEmpty(head(fa.head), for {v <- fa.list} yield head(v))

      case NonEmpty(head, x :: xt) =>
        val h = ap(fa)(NonEmpty(head, Nil))
        val t = ap(fa)(NonEmpty(x, xt))
        NonEmpty(h.head, h.list ::: (t.head :: t.list))
    }

    override def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = NonEmpty.functor.map(fa)(f)
  }
  lazy val monad: Monad[NonEmpty] = new Monad[NonEmpty] {
    override def point[A](a: A): NonEmpty[A] = NonEmpty(a, List())

    override def flatMap[A, B](fa: NonEmpty[A])(f: A => NonEmpty[B]): NonEmpty[B] = fa match {
      case NonEmpty(head, Nil) => f(head)
      case NonEmpty(head, x :: xt) =>
        val h = f(head)
        val t = flatMap(NonEmpty(x, xt))(f)
        NonEmpty(h.head, h.list ::: (t.head :: t.list))
    }
  }
}
