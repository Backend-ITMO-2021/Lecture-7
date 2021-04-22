package ru.ifmo.backend_2021

sealed trait Nat
case object Z extends Nat
case class Succ(s: Nat) extends Nat

object Nat {
  val natNum: Num[Nat] = ???
  val natEq: Eq[Nat] = ???
}
