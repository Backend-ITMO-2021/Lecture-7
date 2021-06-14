package ru.ifmo.backend_2021

import scala.annotation.tailrec

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat

object Nat {
  lazy val natNum: Num[Nat] = new Num[Nat] {
    @tailrec
    def +(a: Nat, b: Nat): Nat = (a, b) match {
      case (_, Z) => a
      case (Z, _) => b
      case (_, Succ(sb)) => this.+(Succ(a), sb)
    }

    def *(a: Nat, b: Nat): Nat = (a, b) match {
      case (Z, Z) | (Z, _) | (_, Z) => Z
      case (_, Succ(Z)) => a
      case (Succ(Z), _) => b
      case (_, Succ(sb)) => this.+(a, this.*(a, sb))
    }

    @tailrec
    def -(a: Nat, b: Nat): Nat = (a, b) match {
      case (_, Z) => a
      case (Z, _) => Z
      case (Succ(sa), Succ(sb)) => this.-(sa, sb)
    }
  }

  lazy val natEq: Eq[Nat] = new Eq[Nat] {
    @tailrec
    override def eq(a: Nat, b: Nat): Boolean = (a, b) match {
      case (Succ(as), Succ(bs)) => this.eq(as, bs)
      case (Z, Z) => true
      case (_, _) => false
    }
  }
}
