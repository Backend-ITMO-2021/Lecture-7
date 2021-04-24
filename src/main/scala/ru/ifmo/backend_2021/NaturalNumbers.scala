package ru.ifmo.backend_2021

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat

object Nat {
  lazy val natNum: Num[Nat] = ???
  lazy val natEq: Eq[Nat] = ???
}
