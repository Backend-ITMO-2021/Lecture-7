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
    override def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = fa match {
      case NonEmpty(head, tail) => NonEmpty(f(head), tail.map(f))
    }
  }
  lazy val applicative: Applicative[NonEmpty] = new Applicative[NonEmpty] {
    override def point[A](a: A): NonEmpty[A] = NonEmpty(a, List.empty)

    override def ap[A, B](fa: NonEmpty[A])(f: NonEmpty[A => B]): NonEmpty[B] = {
      val res = (f.head +: f.list).flatMap((fa.head +: fa.list).map)
      NonEmpty(res.head, res.tail)
    }

    override def map[A, B](fa: NonEmpty[A])(f: A => B): NonEmpty[B] = NonEmpty.functor.map(fa)(f)
  }
  lazy val monad: Monad[NonEmpty] = new Monad[NonEmpty] {
    override def point[A](a: A): NonEmpty[A] = NonEmpty.applicative.point(a)

    override def flatMap[A, B](fa: NonEmpty[A])(f: A => NonEmpty[B]): NonEmpty[B] = {
      NonEmpty.functor.map(fa)(f) match {
        case NonEmpty(NonEmpty(head, list), tail) =>
          NonEmpty(head, list ++ tail.flatMap { case NonEmpty(head, tail) => head +: tail })
      }
    }
  }
}