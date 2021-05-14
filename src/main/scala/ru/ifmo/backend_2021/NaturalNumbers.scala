package ru.ifmo.backend_2021

import scala.annotation.tailrec

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat

object Nat {
  lazy val natNum: Num[Nat] = new Num[Nat] {

    def +(a: Nat, b: Nat): Nat = (a, b) match {
      case (Succ(as), Succ(bs)) => Succ(natNum.+(Succ(as), bs))
      case (_, Z) => a
      case (Z, _) => b
    }

    def -(a: Nat, b: Nat): Nat = (a, b) match {
      case (Succ(as), Succ(bs)) => natNum.-(as, bs)
      case (_, Z) => a
      case (Z, _) => Z
    }

    def *(a: Nat, b: Nat): Nat = (a, b) match {
      case (Succ(_), Succ(s)) => natNum.+(natNum.*(a, s), a)
      case (_, Z) => Z
      case (Z, _) => Z
    }
  }

  lazy val natEq: Eq[Nat] = new Eq[Nat] {

    @tailrec
    def eq(a: Nat, b: Nat): Boolean = (a, b) match {
      case (Succ(as), Succ(bs)) => eq(as, bs)
      case (Z, Z) => true
      case _ => false
    }
  }
}
