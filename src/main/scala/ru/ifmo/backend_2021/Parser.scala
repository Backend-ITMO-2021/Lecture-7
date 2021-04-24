package ru.ifmo.backend_2021

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {
  lazy val functor: Functor[Parser] = ???
  lazy val applicative: Applicative[Parser] = ???
  lazy val alternative: Alternative[Parser] = ???
  lazy val monad: Monad[Parser] = ???

  lazy val ok: Parser[Unit] = ???
  lazy val eof: Parser[Unit] = ???
  lazy val satisfy: (Char => Boolean) => Parser[Char] = ???
  lazy val element: Char => Parser[Char] = ???
  lazy val stream: String => Parser[String] = ???

  lazy val brackets: Parser[Unit] = ???
  lazy val integer: Parser[Int] = ???
}
