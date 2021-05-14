package ru.ifmo.backend_2021

import scala.annotation.tailrec

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat

object Nat {
  lazy val natNum: Num[Nat] = new Num[Nat] {
    @tailrec
    private def sum(y: Nat, acc: Nat): Nat = y match {
      case Z => acc
      case Succ(x) => sum(x, Succ(acc))
    }

    @tailrec
    private def mult(x: Nat, y: Nat, acc: Nat): Nat = y match {
      case Z => acc
      case Succ(s) => mult(x, s, sum(acc, x))
    }

    @tailrec
    private def sub(reduced: Nat, subtracted: Nat): Nat = (reduced, subtracted) match {
      case (Z, _) => Z
      case (a, Z) => a
      case (Succ(a), Succ(b)) => sub(a, b)
    }

    override def +(a: Nat, b: Nat): Nat = sum(a, b)

    override def *(a: Nat, b: Nat): Nat = mult(a, b, Z)

    override def -(a: Nat, b: Nat): Nat = sub(a, b)
  }
  lazy val natEq: Eq[Nat] = new Eq[Nat] {
    override def eq(a: Nat, b: Nat): Boolean = (Nat.natNum.-(a, b), Nat.natNum.-(b, a)) match {
      case (Z, Z) => true
      case _ => false
    }
  }
}