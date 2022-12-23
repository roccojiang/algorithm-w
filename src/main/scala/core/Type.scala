package core

import BasicType.*
import Expr.*

sealed trait Type:
  /** Checks if the given type variable is in the type. */
  def occurs(phi: TVar): Boolean

  /** Returns the set of free type variables that occur in the type. */
  def fv: Set[TVar]

/** Basic types. */
enum BasicType extends Type:
  case TVar(x: Int)
  case TFun(a: BasicType, b: BasicType)
  case TConst(c: TypeConst)

  override def occurs(phi: TVar): Boolean = this match
    case TVar(x)    => TVar(x) == phi
    case TFun(a, b) => a.occurs(phi) || b.occurs(phi)
    case c: TConst  => false

  override def fv: Set[TVar] = this match
    case phi: TVar  => Set(phi)
    case TFun(a, b) => a.fv union b.fv
    case c: TConst  => Set.empty

  override def toString: String = this match
    case TVar(x)    => s"φ$x"
    case TFun(a, b) => s"($a -> $b)"
    case TConst(c)  => c.toString

/** Polymorphic types. */
case class PolyType(vars: Set[TVar], a: BasicType) extends Type:
  override def fv: Set[TVar] = a.fv diff vars

  override def occurs(phi: TVar): Boolean =
    vars.contains(phi) || a.occurs(phi)

  override def toString: String = s"(∀${vars.mkString}.$a)"

object PolyType:
  def apply(t: BasicType): PolyType = PolyType(Set.empty, t)

/** Type constants. */
enum TypeConst:
  case TInt
  case TChar
  case TBool

  override def toString(): String = this match
    case TInt  => "Int"
    case TChar => "Char"
    case TBool => "Bool"
