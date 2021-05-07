package ru.ifmo.backend_2021

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {

  lazy val functor: Functor[Parser] = new Functor[Parser] {

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      new Parser[B](fa.runParser(_).map { (p: (A, String)) => (f(p._1), p._2) })
  }

  lazy val applicative: Applicative[Parser] = new Applicative[Parser] {

    override def point[A](a: A): Parser[A] = new Parser[A]({ s => Option((a, s)) })

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = new Parser[B](
      f.runParser(_).flatMap(f =>
        fa.runParser(f._2).map(fa =>
          (f._1(fa._1), fa._2)
        )
      )
    )

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser.functor.map(fa)(f)
  }

  lazy val alternative: Alternative[Parser] = new Alternative[Parser] {

    override def empty[A]: Parser[A] = new Parser[A]({ _ => Option.empty })

    override def orElse[A](fa: Parser[A], recover: => Parser[A]): Parser[A] = new Parser[A]({ s =>
      fa.runParser(s).orElse(recover.runParser(s))
    })

    override def point[A](a: A): Parser[A] = applicative.point(a)

    override def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = applicative.ap(fa)(f)

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = functor.map(fa)(f)
  }

  lazy val monad: Monad[Parser] = new Monad[Parser] {

    override def point[A](a: A): Parser[A] = applicative.point(a)

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = new Parser[B](
      fa.runParser(_).flatMap({ fa =>
        f(fa._1).runParser(fa._2)
      })
    )
  }

  lazy val ok: Parser[Unit] = new Parser[Unit]({ s => Some(((), s)) })

  lazy val eof: Parser[Unit] = new Parser[Unit]({ s =>
    if (s.isEmpty) Some(((), ""))
    else None
  })

  def satisfy(predicate: Char => Boolean): Parser[Char] = new Parser[Char]({ s =>
    if (s.nonEmpty && predicate(s(0))) Some((s(0), s.substring(1)))
    else None
  })

  lazy val element: Char => Parser[Char] = { c => satisfy(_ == c) }

  lazy val stream: String => Parser[String] = { s =>
    s.map(element).foldRight(monad.point(""))((p, acc) =>
      monad.flatMap(p)({ c =>
        functor.map(acc)(c + _)
      })
    )
  }

  lazy val ab: Parser[Unit] = alternative.orElse(eof, monad.flatMap(stream("ab")) { _ => ab })

  lazy val sign: Parser[Char] = satisfy("+-".contains(_))

  lazy val digit: Parser[Char] = satisfy(_.isDigit)

  lazy val unsigned: Parser[Int] = {
    lazy val str: Parser[String] = monad.flatMap(digit) { head =>
      alternative.orElse(
        functor.map(str) { tail => head + tail },
        functor.map(ok) { _ => head.toString }
      )
    }

    functor.map(str) { s => s.toInt }
  }

  def fullParse[A](p: Parser[A]): Parser[A] =
    monad.flatMap(p) { x =>
      functor.map(eof)(_ => x)
    }

  lazy val integer: Parser[Int] = fullParse(monad.flatMap(
    alternative.orElse(
      functor.map(sign) { s => if (s == '-') -1 else 1 },
      functor.map(ok)(_ => 1)
    )
  ) { sign =>
    functor.map(unsigned)(_ * sign)
  })

  lazy val brackets: Parser[Unit] = {
    lazy val chunk: Parser[Unit] = alternative.orElse(
      monad.flatMap(
        monad.flatMap(element('(')) { _ =>
          monad.flatMap(chunk) {
            _ => functor.map(element(')'))(_ => ())
          }
        }
      ) { _ => chunk },
      ok
    )

    fullParse(chunk)
  }
}
