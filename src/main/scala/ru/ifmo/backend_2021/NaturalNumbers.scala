package ru.ifmo.backend_2021

sealed trait Nat

case object Z extends Nat

case class Succ(s: Nat) extends Nat

object Nat {
  lazy val natNum: Num[Nat] = new Num[Nat] {
  	override def +(a: Nat, b: Nat): Nat = {
  		(a, b) match {
  			case (x, Z) => x
  			case (Z, y) => y
  			case (x, Succ(y)) => this.+(Succ(x), y)
  		}
  	}

  	override def *(a: Nat, b: Nat): Nat = {
  		(a, b) match {
  			case (x, Z) => Z
  			case (Z, y) => Z
  			case (x, Succ(Z)) => x
  			case (Succ(Z), y) => y
  			case (x, Succ(y)) => this.+(this.*(x, y), x)
  		}
  	}

  	override def -(a: Nat, b: Nat): Nat = {
  		(a, b) match {
  			case (x, Z) => x
  			case (Z, y) => Z
  			case (Succ(x), Succ(y)) => this.-(x,y)
  		}
  	}

  }
  lazy val natEq: Eq[Nat] = new Eq[Nat] {
  	override def eq(a: Nat, b: Nat): Boolean = {
  		(a, b) match {
  			case (Z, Z) => true
  			case (Succ(x), Succ(y)) => this.eq(x, y)
  			case _ => false
  		}
  	}
  }
}
