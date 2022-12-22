package core

import BasicType.*

sealed trait Type:
  /** Checks if the given type variable is in the type. */
  def occurs(phi: TVar): Boolean

  /** Returns the set of free type variables that occur in the type. */
  def fv: Set[TVar]

/** Basic types. */
enum BasicType extends Type:
  case TVar(x: Int)
  case TFun(a: BasicType, b: BasicType)

  override def occurs(phi: TVar): Boolean = this match
    case TVar(x)    => TVar(x) == phi
    case TFun(a, b) => a.occurs(phi) || b.occurs(phi)

  override def fv: Set[TVar] = this match
    case phi: TVar  => Set(phi)
    case TFun(a, b) => a.fv union b.fv

  override def toString: String = this match
    case TVar(x)  => s"φ$x"
    case a TFun b => s"($a -> $b)"

/** Polymorphic types. */
case class PolyType(vars: Set[TVar], a: BasicType) extends Type:
  override def fv: Set[TVar] = a.fv diff vars

  override def occurs(phi: TVar): Boolean =
    vars.contains(phi) || a.occurs(phi)

  override def toString: String = s"(∀${vars.mkString}.$a)"

object PolyType:
  def apply(t: BasicType): PolyType = PolyType(Set.empty, t)
