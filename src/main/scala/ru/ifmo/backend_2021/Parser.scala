package ru.ifmo.backend_2021

case class Parser[A](runParser: String => Option[(A, String)])

object Parser {
  lazy val functor: Functor[Parser] = new Functor[Parser] {
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser(x => fa.runParser(x).map { case (a, str) => (f(a), str) })
  }

  lazy val applicative: Applicative[Parser] = new Applicative[Parser] {
    def point[A](a: A): Parser[A] = Parser(x => Option(a, x))
    def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = Parser(x => f.runParser(x).flatMap { case (a_b, str) => map(fa)(a_b).runParser(x) })
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = functor.map(fa)(f)
  }

  lazy val monad: Monad[Parser] = new Monad[Parser] {
    def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = Parser(x => fa.runParser(x).flatMap { case (a, str) => f(a).runParser(str) })
    def point[A](a: A): Parser[A] = applicative.point(a)
  }

  lazy val alternative: Alternative[Parser] = new Alternative[Parser] {
    def empty[A]: Parser[A] = Parser(_ => None)
    def orElse[A](fa: Parser[A], recover: => Parser[A]): Parser[A] = Parser(x => fa.runParser(x).orElse( recover.runParser(x) ))
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = applicative.map(fa)(f)
    def point[A](a: A): Parser[A] = applicative.point(a)
    def ap[A, B](fa: Parser[A])(f: Parser[A => B]): Parser[B] = applicative.ap(fa)(f)
  }

  lazy val ok: Parser[Unit] = Parser(x => Some((), x))

  lazy val eof: Parser[Unit] = Parser(x => x match {
    case _ if x.length > 0 => None
    case _ => Some((), x)
  })

  def satisfy(predicate: Char => Boolean): Parser[Char] = Parser(x => x match {
    case _ if x.length > 0 && predicate(x.head) => Some(x.head, x.drop(1))
    case _ => None
  })

  lazy val element: Char => Parser[Char] = char => satisfy(_ == char)

  lazy val stream: String => Parser[String] = str => Parser(x => x match {
    case _ if x.startsWith(str) => Some(str, x.drop(str.length))
    case _ => None
  })

  lazy val ab: Parser[Unit] = alternative.orElse(
    monad.flatMap(stream("ab"))(_ => ab),
    eof
  )


  // Integer

  def getRank(n: Int): Int = {
    if (n == 0) 0 else getRank(n / 10) + 1
  }

  lazy val digits: Parser[Int] = alternative.orElse(
    monad.flatMap
      ( satisfy("1234567890".contains(_)) )
      ( digit => functor.map(digits)(num => digit.asDigit * Math.pow(10, getRank(num)).toInt + num) ),
    functor.map(eof)(_ => 0)
  )

  lazy val integer: Parser[Int] = monad.flatMap(
      alternative.orElse(
        alternative.orElse(
          functor.map(element('-'))(_ => -1),
          functor.map(element('+'))(_ => 1)
        ),
        functor.map(ok)(_ => 1)
      )
    )
    { sign => functor.map(digits)(sign * _) }

  
  // Brackets

  lazy val bracketsCalc: Parser[Int] = alternative.orElse(
    monad.flatMap(
      alternative.orElse(
        functor.map(element('('))(_ => -1),
        functor.map(element(')'))(_ => 1)
      ))
      { counter => {
        monad.flatMap(bracketsCalc)(sum => {
          val s = counter + sum
          if (s < 0) alternative.empty[Int] else applicative.point[Int](s)
        })

      } },
    functor.map(eof)(_ => 0)
  )
  
  lazy val brackets: Parser[Unit] = monad.flatMap(bracketsCalc) {
    case 0 => ok
    case _ => alternative.empty[Unit]
  }
}
