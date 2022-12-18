package core

import core.Expr._
import core.BasicType._

import scala.collection.immutable

type Context = Map[EVar, Type]

// TODO: can substitutions only map to basic types?
case class Subst(subMap: Map[Type, BasicType]):
  def apply(t: Type): Type =
    def applyBasic(t: BasicType): BasicType = t match
      case phi: TVar => subMap.getOrElse(phi, phi)
      case a TArr b  => TArr(applyBasic(a), applyBasic(b))

    t match
      case PolymorphicType(quantified, t) => ???
      case t: BasicType                   => applyBasic(t)

  def apply(c: Context): Context = c.map((x, a) => x -> apply(a))

  def apply(pp: (Context, Type)): (Context, Type) =
    val (c, t) = pp
    (apply(c), apply(t))

  def compose(s: Subst): Subst = Subst(subMap ++ s.subMap)

object Subst:
  def apply(sub: (Type, BasicType)): Subst = Subst(Map(sub))
  def id: Subst = Subst(Map.empty)
