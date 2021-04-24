package ru.ifmo.backend_2021

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {
  val functor: Functor[Parser] = ???
  val applicative: Applicative[Parser] = ???
  val alternative: Alternative[Parser] = ???
  val monad: Monad[Parser] = ???

  val ok: Parser[()] = ???
  val eof: Parser[()] = ???
  val satisfy: (Char => Boolean) => Parser[Char] = ???
  val element: Char => Parser[Char] = ???
  val stream: String => Parser[String] = ???

  val brackets: Parser[()] = ???
  val integer: Parser[Int] = ???
}