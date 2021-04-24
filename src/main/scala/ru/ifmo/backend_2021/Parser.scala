package ru.ifmo.backend_2021

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {
  lazy val functor: Functor[Parser] = ???
  lazy val applicative: Applicative[Parser] = ???
  lazy val alternative: Alternative[Parser] = ???
  lazy val monad: Monad[Parser] = ???

  lazy val ok: Parser[Unit] = ???
  lazy val eof: Parser[Unit] = ???
  def satisfy(predicate: Char => Boolean): Parser[Char] = ???
  lazy val element: Char => Parser[Char] = ???
  lazy val stream: String => Parser[String] = ???

  lazy val ab: Parser[Unit] = ???
  lazy val integer: Parser[Int] = ???
  lazy val brackets: Parser[Unit] = ???
}
