package ru.ifmo.backend_2021

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat


object Nat {
  lazy val natNum: Num[Nat] = new Num[Nat] {
    def +(a: Nat, b: Nat): Nat = (a, b) match {
      case (_, Z) => a
      case (_, Succ(y)) => natNum.+(Succ(a), y)
    }

    def *(a: Nat, b: Nat): Nat = (a, b) match {
      case (_, Z) => Z
      case (Z, _) => Z
      case (Succ(_), Succ(Z)) => a
      case (Succ(_), Succ(y)) => natNum.+(natNum.*(a, y), a)
    }

    def -(a: Nat, b: Nat): Nat = (a, b) match {
      case (Z, _) => Z
      case (Succ(_), Z) => a
      case (Succ(x), Succ(y)) => natNum.-(x, y)
    }
  }

  lazy val natEq: Eq[Nat] = (a: Nat, b: Nat) => (a, b) match {
    case (Z, Z) => true
    case (Succ(x), Succ(y)) => natEq.eq(x, y)
    case _ => false
  }
}
