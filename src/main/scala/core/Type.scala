package core

import TBasic._

sealed trait Type:
  /** Checks if the given type variable is in the type. */
  def occurs(phi: TVar): Boolean

  /** Returns the set of free type variables that occur in the type. */
  def fv: Set[TVar]

/** Basic types. */
enum TBasic extends Type:
  case TVar(x: Int)
  case TArr(a: TBasic, b: TBasic)

  override def occurs(phi: TVar): Boolean = this match
    case TVar(x)    => TVar(x) == phi
    case TArr(a, b) => a.occurs(phi) || b.occurs(phi)

  override def fv: Set[TVar] = this match
    case phi: TVar  => Set(phi)
    case TArr(a, b) => a.fv union b.fv

  override def toString: String = this match
    case TVar(x)  => s"φ$x"
    case a TArr b => s"($a -> $b)"

/** Polymorphic types. */
case class TPoly(vars: Set[TVar], t: TBasic) extends Type:
  override def fv: Set[TVar] = t.fv diff vars

  override def occurs(phi: TVar): Boolean =
    vars.contains(phi) || t.occurs(phi)

  override def toString: String = s"(∀${vars.mkString}.$t)"
