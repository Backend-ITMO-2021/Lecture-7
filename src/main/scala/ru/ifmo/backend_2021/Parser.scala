package ru.ifmo.backend_2021

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {
  lazy val functor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = {
      Parser(fa.runParser.andThen(_.map { case (ans, tail) => (f(ans), tail) }))
    }
  }

  lazy val applicative: Applicative[Parser] = new Applicative[Parser] {
    override def point[A](a: A): Parser[A] = Parser(string => Some(a, string))

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = {
      Parser(string =>
        f.runParser(string).flatMap(resF => fa.runParser(resF._2).map(res => (resF._1(res._1), res._2)))
      )
    }

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = functor.map(fa)(f)
  }
  lazy val alternative: Alternative[Parser] = new Alternative[Parser] {

    override def empty[A]: Parser[A] = Parser(_ => None)

    override def orElse[A](fa: Parser[A], recover: => Parser[A]): Parser[A] = {
      Parser(string => fa.runParser(string).orElse(recover.runParser(string)))
    }

    override def point[A](a: A): Parser[A] = applicative.point(a)

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = applicative.ap(fa)(f)

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = applicative.map(fa)(f)
  }
  lazy val monad: Monad[Parser] = new Monad[Parser] {
    override def point[A](a: A): Parser[A] = applicative.point(a)


    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = {
      Parser(string => fa.runParser(string).flatMap(res => f(res._1).runParser(res._2)))
    }
  }

  lazy val ok: Parser[Unit] = applicative.point()
  lazy val eof: Parser[Unit] = Parser(string => if (string == "") Some((), "") else None)


  def satisfy(predicate: Char => Boolean): Parser[Char] = Parser {
    case string if predicate(string.charAt(0)) => Some(string.charAt(0), string.drop(1))
    case _ => None
  }

  lazy val element: Char => Parser[Char] = char => satisfy(_ == char)
  lazy val stream: String => Parser[String] = string => Parser { text =>
    if (text.startsWith(string)) {
      Some(string, text.drop(string.length))
    } else {
      None
    }
  }

  lazy val ab: Parser[Unit] = alternative.orElse(monad.flatMap(stream("ab"))(_ => ab), eof)


  lazy val opt: String => Parser[String] = { string =>
    alternative.orElse(functor.map(satisfy(string.contains(_)))(_.toString), Parser(str => Some("", str)))
  }


  lazy val digit: Parser[String] = functor.map(satisfy("1234567890".contains(_)))(_.toString)


  lazy val unsignedNumber: Parser[String] = {
    alternative.orElse(
      functor.map(eof)(_ => ""), monad.flatMap(digit)((res: String) => functor.map(unsignedNumber)(res ++ _))
    )
  }

  lazy val integer: Parser[Int] = {
    functor.map(monad.flatMap(opt("+-"))(res => functor.map(unsignedNumber)(res ++ _)))(_.toInt)
  }

  lazy val mapBracketToNumber: Parser[Int] = {
    functor.map(satisfy("()".contains(_)))((x: Char) => if (x == '(') 1 else -1)
  }

  lazy val balanceCounter: Parser[Int] = {
    alternative.orElse(
      functor.map(eof)(_ => 0),
      monad.flatMap(mapBracketToNumber)((res: Int) => {
        monad.flatMap(balanceCounter)((x: Int) => if (res + x > 0) alternative.empty else functor.map(ok)(_ => res + x))
      })
    )
  }

  lazy val brackets: Parser[Unit] = {
    monad.flatMap(balanceCounter)(ans => if (ans == 0) ok else alternative.empty)
  }
}