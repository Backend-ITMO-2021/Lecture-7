package ru.ifmo.backend_2021

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat

object Nat {
  lazy val natNum: Num[Nat] = new Num[Nat] {
    override def +(a: Nat, b: Nat): Nat = (a, b) match {
      case (Z, Z) => Z
      case (Z, b: Succ) => Succ(this.+(Z, b.s))
      case (a: Succ, Z) => Succ(this.+(a.s, Z))
      case (a: Succ, b: Succ) => Succ(this.+(a.s, b))
    }

    override def *(a: Nat, b: Nat): Nat = (a, b) match {
      case (Z, Z) | (Z, _: Succ) | (_: Succ, Z) => Z
      case (a: Succ, b: Succ) => if (b.s == Z) a else this.+(a, this.*(a, b.s))
    }

    @scala.annotation.tailrec
    override def -(a: Nat, b: Nat): Nat = (a, b) match {
      case (Z, Z) | (Z, _: Succ) => Z
      case (a: Succ, Z) => a
      case (a: Succ, b: Succ) => this.-(a.s, b.s)
    }
  }
  lazy val natEq: Eq[Nat] = (a: Nat, b: Nat) => (a, b) match {
    case (Z, Z) => true
    case (Z, _: Succ) | (_: Succ, Z) => false
    case (a: Succ, b: Succ) => eq(a.s, b.s)
  }
}
