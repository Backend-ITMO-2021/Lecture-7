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
      case (_, Z) => Z
      case (Z, _) => Z
      case (Succ(_), Succ(Z)) => a
      case (Succ(_), Succ(y)) => natNum.+(natNum.*(a, y), a)
    }

    override def -(a: Nat, b: Nat): Nat = (a, b) match {
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
