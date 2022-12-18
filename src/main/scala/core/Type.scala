package core

import BasicType._

sealed trait Type:
  def occurs(phi: TVar): Boolean

enum BasicType extends Type:
  case TVar(x: Int)
  case TArr(a: BasicType, b: BasicType)

  override def occurs(phi: TVar): Boolean = this match
    case TVar(x)    => TVar(x) == phi
    case TArr(a, b) => a.occurs(phi) || b.occurs(phi)

  override def toString(): String = this match
    case TVar(x)  => s"φ$x"
    case a TArr b => s"($a -> $b)"

case class PolymorphicType(quantified: Set[TVar], t: BasicType) extends Type:
  override def occurs(phi: TVar): Boolean = ??? // TODO

  override def toString(): String = s"(∀${quantified.mkString}.$t)"
