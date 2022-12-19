package core

import TBasic._

sealed trait Type:
  def occurs(phi: TVar): Boolean

/** Basic types. */
enum TBasic extends Type:
  case TVar(x: Int)
  case TArr(a: TBasic, b: TBasic)

  override def occurs(phi: TVar): Boolean = this match
    case TVar(x)    => TVar(x) == phi
    case TArr(a, b) => a.occurs(phi) || b.occurs(phi)

  override def toString: String = this match
    case TVar(x)  => s"φ$x"
    case a TArr b => s"($a -> $b)"

/** Polymorphic types. */
case class TPoly(quantified: Set[TVar], t: TBasic) extends Type:
  override def occurs(phi: TVar): Boolean =
    quantified.contains(phi) || t.occurs(phi)

  override def toString: String = s"(∀${quantified.mkString}.$t)"
