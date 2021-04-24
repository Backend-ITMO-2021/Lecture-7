package ru.ifmo.backend_2021

object AlternativeExample {
  lazy val optionAlternative = new Alternative[Option] {
    def empty[A]: Option[A] = None
    def orElse[A](fa: Option[A], recover: Option[A]): Option[A] =
      fa match {
        case _: Some[_] => fa
        case None => recover
      }

    // See lecture notes for these methods
    def point[A](a: A): Option[A] = ???
    def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = ???
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = ???
  }
}
