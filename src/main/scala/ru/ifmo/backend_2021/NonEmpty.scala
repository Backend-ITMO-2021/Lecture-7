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
      NonEmpty(f(fa.head), fa.list.map(f))
  }

  lazy val applicative: Applicative[NonEmpty] = new Applicative[NonEmpty] {

    override def point[A](a: A): NonEmpty[A] = NonEmpty(a, List.empty)

    override def ap[A, B](fa: NonEmpty[A])(f: NonEmpty[A => B]): NonEmpty[B] = f match {
      case NonEmpty(f, f1 :: fs) =>
        val x = ap(fa)(NonEmpty(f1, fs))
        val y = map(fa)(f)

        NonEmpty(y.head, y.list ++ (x.head :: x.list))

      case NonEmpty(f, _) => map(fa)(f)
    }

    override def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] =
      NonEmpty.functor.map(fa)(f)
  }

  lazy val monad: Monad[NonEmpty] = new Monad[NonEmpty] {

    override def point[A](a: A): NonEmpty[A] = NonEmpty.applicative.point(a)

    override def flatMap[A, B](fa: NonEmpty[A])(f: A => NonEmpty[B]): NonEmpty[B] = fa match {
      case NonEmpty(x, x1 :: xs) =>
        val y = f(x)
        val ys = flatMap(NonEmpty(x1, xs))(f)

        NonEmpty(y.head, y.list ++ (ys.head :: ys.list))
      case NonEmpty(x, _) => f(x)
    }
  }
}
