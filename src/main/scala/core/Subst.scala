package core

import Expr.*
import BasicType.*

import scala.collection.immutable

type Context = Map[EVar, PolyType]

case class Subst(subst: Map[Type, BasicType]):

  def apply(t: BasicType): BasicType = t match
    case phi: TVar  => subst.getOrElse(phi, phi)
    case TFun(a, b) => TFun(apply(a), apply(b))
    case c: TConst  => c

  def apply(t: PolyType): PolyType =
    PolyType(t.vars, apply(t.a))

  def apply(c: Context): Context = c.map((x, a) => x -> apply(a))

  def compose(s2: Subst): Subst =
    val s1 = this // (s1 . s2): just for clarity
    Subst(s1.subst ++ s2.subst.mapValues(s1(_)).toMap)

object Subst:
  def apply(sub: (Type, BasicType)): Subst = Subst(Map(sub))
  def id: Subst = Subst(Map.empty)
