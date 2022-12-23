package ml.inference

import BasicType.*
import ml.ast.Expr

/** Typeclass expressing common behaviour between types and type contexts. */
trait Types[T]:
  /** Returns the set of free type variables that occur in the type. */
  extension (t: T) def fv: Set[TVar]

  /** Applies the given substitution to the type. */
  extension (t: T) def subst(s: Subst): T

/** Basic (monomorphic) types. */
enum BasicType:
  case TVar(x: Int)
  case TFun(a: BasicType, b: BasicType)
  case TConst(c: TypeConst)

  /** Checks if the given type variable occurs in the type. */
  def contains(phi: TVar): Boolean = this match
    case TVar(x)    => TVar(x) == phi
    case TFun(a, b) => a.contains(phi) || b.contains(phi)
    case c: TConst  => false

  override def toString: String = this match
    case TVar(x)    => s"φ$x"
    case TFun(a, b) => s"($a -> $b)"
    case TConst(c)  => c.toString

given Types[BasicType] with
  extension (t: BasicType)
    def fv = t match
      case phi: TVar  => Set(phi)
      case TFun(a, b) => a.fv union b.fv
      case c: TConst  => Set.empty

    def subst(s: Subst) = t match
      case phi: TVar  => s.getOrElse(phi, phi)
      case TFun(a, b) => TFun(s(a), s(b))
      case c: TConst  => c

/** Polymorphic types. */
case class PolyType(vars: Set[TVar], a: BasicType):
  override def toString: String = s"(∀${vars.mkString}.$a)"

object PolyType:
  def apply(t: BasicType): PolyType = PolyType(Set.empty, t)

given Types[PolyType] with
  extension (t: PolyType)
    def fv = t.a.fv diff t.vars
    def subst(s: Subst) = PolyType(t.vars, s(t.a))

/** Type contexts, mapping term variables to their (polymorphic) types. */
type Context = Map[Expr.EVar, PolyType]

given Types[Context] with
  extension (c: Context)
    def fv = c.values.flatMap(_.fv).toSet
    def subst(s: Subst) = c.map((x, a) => x -> s(a))

/** Type constants. */
enum TypeConst:
  case TInt
  case TChar
  case TBool

  override def toString(): String = this match
    case TInt  => "Int"
    case TChar => "Char"
    case TBool => "Bool"

given Conversion[TypeConst, TConst] = TConst(_)
