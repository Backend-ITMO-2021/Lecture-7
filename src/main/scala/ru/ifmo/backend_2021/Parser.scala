package ru.ifmo.backend_2021

import scala.util.matching.Regex

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {
  lazy val functor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser(fa.runParser(_).map { case (a, x) => (f(a), x) })
  }
  lazy val applicative: Applicative[Parser] = new Applicative[Parser] {
    override def point[A](a: A): Parser[A] = Parser(x => Option(a, x))

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = Parser(x =>
      f.runParser(x).flatMap { case (f, a) => map(fa)(f).runParser(a) })

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser.functor.map(fa)(f)
  }
  lazy val alternative: Alternative[Parser] = new Alternative[Parser] {
    override def empty[A]: Parser[A] = Parser({ _ => None })

    override def orElse[A](fa: Parser[A], recover: => Parser[A]): Parser[A] = Parser(x => fa.runParser(x).orElse(recover.runParser(x)))

    override def point[A](a: A): Parser[A] = Parser.applicative.point(a)

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = Parser.applicative.ap(fa)(f)

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser.functor.map(fa)(f)
  }
  lazy val monad: Monad[Parser] = new Monad[Parser] {
    override def point[A](a: A): Parser[A] = Parser.applicative.point(a)

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = Parser(
      fa.runParser(_).flatMap { case(a, x) => f(a).runParser(x)})
  }

  lazy val ok: Parser[Unit] = Parser({Some((), _)})
  lazy val eof: Parser[Unit] = Parser( x => if (x.nonEmpty) None else Some(((), x)))

  def satisfy(predicate: Char => Boolean): Parser[Char] = Parser(x =>
  if (x.nonEmpty && predicate(x.head)) Some((x.head, x.substring(1))) else None)

  lazy val element: Char => Parser[Char] = c => satisfy(_ == c)

  lazy val stream: String => Parser[String] = str => Parser(x =>
  if (x.startsWith(str)) Some((str, x.substring(str.length))) else None)

  lazy val ab: Parser[Unit] = alternative.orElse(eof, monad.flatMap(stream("ab"))(_ => ab))

  lazy val finder: Regex => Parser[String] = pattern => Parser(x =>
    pattern.findFirstMatchIn(x) match {
      case Some(x) => Some((x.toString(), ""))
      case None => None
    } )

  lazy val integer: Parser[Int] = alternative.orElse(
    monad.flatMap(finder(new Regex("^(\\-|\\+)?\\d*$")))(
      a => alternative.point(a.toInt)),
    alternative.empty)


  lazy val checker: Parser[Int] = alternative.orElse(
    functor.map(eof)(_ => 0),
    monad.flatMap(
      alternative.orElse(
        functor.map(element('('))(_ => -1),
        functor.map(element(')'))(_ => 1)
      ))
    { c => monad.flatMap(checker)(x  => if (c + x < 0) alternative.empty else functor.map(ok)(_ => c + x))}
  )

  lazy val brackets: Parser[Unit] = monad.flatMap(checker) { x =>
    if (x == 0) ok else alternative.empty[Unit]}
}
