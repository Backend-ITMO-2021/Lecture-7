package ru.ifmo.backend_2021

import scala.annotation.tailrec

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat

object Nat {

  lazy val natNum: Num[Nat] = new Num[Nat] {

    @tailrec
    override def +(a: Nat, b: Nat): Nat = a match {
      case Succ(s) => this.+(s, Succ(b))
      case Z => b
    }

    override def *(a: Nat, b: Nat): Nat = a match {
      case Succ(s) => this.+(b, this.*(s, b))
      case Z => Z
    }

    @tailrec
    override def -(a: Nat, b: Nat): Nat = (a, b) match {
      case (Succ(as), Succ(bs)) => this.-(as, bs)
      case (_, Z) => a
      case (Z, _) => Z
    }
  }

  lazy val natEq: Eq[Nat] = new Eq[Nat] {

    @tailrec
    override def eq(a: Nat, b: Nat): Boolean = (a, b) match {
      case (Succ(as), Succ(bs)) => this.eq(as, bs)
      case (Z, Z) => true
      case _ => false
    }
  }
}
