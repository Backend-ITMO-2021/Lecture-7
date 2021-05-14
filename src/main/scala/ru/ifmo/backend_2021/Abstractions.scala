package ru.ifmo.backend_2021

trait Eq[M] {
  def eq(a: M, b: M): Boolean
}

trait Num[M] {
  def +(a: M, b: M): M

  def *(a: M, b: M): M

  def -(a: M, b: M): M
}

trait Semigroup[M] {
  def op(a: M, b: M): M

  trait SemigroupLaws {
    def associative(f1: M, f2: M, f3: M): Boolean =
      op(f1, op(f2, f3)) == op(op(f1, f2), f3)
  }

  lazy val semigroupLaws: SemigroupLaws = new SemigroupLaws {}
}

trait Monoid[M] extends Semigroup[M] {
  def zero: M

  def op(a: M, b: M): M

  trait MonoidLaws {
    def leftIdentity(a: M): Boolean = a == op(zero, a)

    def rightIdentity(a: M): Boolean = a == op(a, zero)
  }

  lazy val monoidLaws: MonoidLaws = new MonoidLaws {}
}

trait Foldable[F[_]] {
  def fold[A](fa: F[A])(implicit F: Monoid[A]): A

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

  def foldr[A, B](fa: F[A], z: B)(f: A => B => B): B

  def toList[A](fa: F[A]): List[A] = foldr(fa, List.empty[A])(a => list => a :: list)

  trait FoldableLaw {
    def rightConsistent[A](fa: F[A]): Boolean =
      foldMap(fa)(Vector(_))(new Monoid[Vector[A]] {
        def zero: Vector[A] = Vector.empty

        def op(a: Vector[A], b: Vector[A]): Vector[A] = a ++ b
      }
      ) == foldr(fa, Vector.empty[A])(a => b => a +: b)
  }

  lazy val foldableLaw: FoldableLaw = new FoldableLaw {}
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  trait FunctorLaws {
    def identity[A](fa: F[A]): Boolean = map(fa)(x => x) == fa

    def composite[A, B, C](fa: F[A], f1: A => B, f2: B => C): Boolean =
      map(map(fa)(f1))(f2) == map(fa)(f1 andThen f2)
  }

  lazy val functorLaws: FunctorLaws = new FunctorLaws {}
}

trait Applicative[F[_]] extends Functor[F] {
  def point[A](a: A): F[A]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  trait ApplicativeLaws {
    def identity[A](fa: F[A]): Boolean =
      fa == ap(fa)(point((a: A) => a))

    def composition[A, B, C](fbc: F[B => C], fab: F[A => B], fa: F[A]): Boolean =
      ap(ap(fa)(fab))(fbc) == ap(fa)(ap(fab)(map(fbc)((bc: B => C) => (ab: A => B) => bc compose ab)))

    def homomorphism[A, B](ab: A => B, a: A): Boolean =
      ap(point(a))(point(ab)) == point(ab(a))

    def interchange[A, B](f: F[A => B], a: A): Boolean =
      ap(point(a))(f) == ap(f)(point((f: A => B) => f(a)))
  }

  lazy val applicativeLaws: ApplicativeLaws = new ApplicativeLaws {}
}

trait Alternative[F[_]] extends Applicative[F] {
  def empty[A]: F[A]

  def orElse[A](fa: F[A], recover: => F[A]): F[A]

  trait AlternativeLaws {
    def identity[A](fa: F[A]): Boolean =
      orElse(empty, fa) == fa

    def associativity[A](a: F[A], b: F[A], c: F[A]): Boolean =
      orElse(orElse(a, b), c) == orElse(a, orElse(b, c))
  }

  lazy val alternativeLaws: AlternativeLaws = new AlternativeLaws {}
}

trait Monad[F[_]] extends Applicative[F] {
  def point[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => point(f(a)))

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(x => map(fa)(x))

  trait MonadLaws {
    def leftIdentity[A, B](a: A, f: A => F[B]): Boolean = flatMap(point(a))(f) == f(a)

    def rightIdentity[A](a: F[A]): Boolean = flatMap(a)(point(_: A)) == a

    def associativity[A, B, C](a: F[A], f: A => F[B], g: B => F[C]): Boolean =
      flatMap(flatMap(a)(f))(g) == flatMap(a)(x => flatMap(f(x))(g))
  }

  lazy val monadLaws: MonadLaws = new MonadLaws {}
}
