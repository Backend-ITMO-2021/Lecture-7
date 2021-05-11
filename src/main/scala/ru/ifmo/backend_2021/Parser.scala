package ru.ifmo.backend_2021

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {
  lazy val functor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser(
      fa.runParser(_).map { case (a, str) => (f(a), str) }
    )
  }
  lazy val applicative: Applicative[Parser] = new Applicative[Parser] {
    override def point[A](a: A): Parser[A] = Parser({ text => Option(a, text) })

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = Parser(text =>
      f.runParser(text).flatMap { case (aToB, str) => map(fa)(aToB).runParser(str) }
    )

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = functor.map(fa)(f)
  }
  lazy val alternative: Alternative[Parser] = new Alternative[Parser] {
    override def empty[A]: Parser[A] = Parser({ _ => None })

    override def orElse[A](fa: Parser[A], recover: => Parser[A]): Parser[A] = Parser({ str =>
      fa.runParser(str).orElse(recover.runParser(str))
    })

    override def point[A](a: A): Parser[A] = applicative.point(a)

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = applicative.ap(fa)(f)

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = functor.map(fa)(f)
  }
  lazy val monad: Monad[Parser] = new Monad[Parser] {
    override def point[A](a: A): Parser[A] = applicative.point(a)

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = Parser(
      fa.runParser(_).flatMap { case (a, str) =>
        f(a).runParser(str)
      }
    )
  }

  lazy val ok: Parser[Unit] = Parser({
    Some((), _)
  })
  lazy val eof: Parser[Unit] = Parser({
    case str@"" => Some(((), str))
    case _ => None
  })

  def satisfy(predicate: Char => Boolean): Parser[Char] = Parser({ str =>
    str.find(predicate) match {
      case Some(value) => Some((value, str.replaceFirst(value.toString, "")))
      case None => None
    }
  })

  def fullySatisfy(pattern: String): Parser[String] = Parser({ str =>
    if (str.matches(pattern)) Some((str, "")) else None
  })

  lazy val element: Char => Parser[Char] = { ch => satisfy(_ == ch) }
  lazy val stream: String => Parser[String] = { str =>
    Parser({ s => if (s.contains(str)) Some((str, s.replace(str, ""))) else None })
  }

  lazy val ab: Parser[Unit] = alternative.orElse(monad.flatMap(stream("ab"))(_ => ab), eof)
  lazy val integer: Parser[Int] = alternative.orElse(
    monad.flatMap(fullySatisfy("^(\\-|\\+)?\\d*$"))(
      a => alternative.point(a.toInt)),
    alternative.empty)
  lazy val brackets: Parser[Unit] = ???
}
