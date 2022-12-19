package core

import Expr._
import TBasic._

import scala.collection.immutable

type Context = Map[EVar, Type]

// TODO: can substitutions only map to basic types?
case class Subst(subMap: Map[Type, TBasic]):
  // TODO: can fix annoying overloading?
  def apply(t: Type): Type = t match
    case t: TBasic => apply(t)
    case t: TPoly  => apply(t)

  def apply(t: TBasic): TBasic = t match
    case phi: TVar => subMap.getOrElse(phi, phi)
    case a TArr b  => TArr(apply(a), apply(b))

  def apply(t: TPoly): TPoly =
    val TPoly(vars, a) = t
    TPoly(vars, apply(a))

  // def apply(t: Type): Type =
  //   def applyBasic(t: TBasic): TBasic = t match
  //     case phi: TVar => subMap.getOrElse(phi, phi)
  //     case a TArr b  => TArr(applyBasic(a), applyBasic(b))

  //   t match
  //     case TPoly(vars, t) => TPoly(vars, applyBasic(t))
  //     case t: TBasic            => applyBasic(t)

  def apply(c: Context): Context = c.map((x, a) => x -> apply(a))

  def apply(pp: (Context, Type)): (Context, Type) =
    val (c, t) = pp
    (apply(c), apply(t))

  def apply(subst: Subst)(t: Type): Type = apply(subst(t))

  def compose(s2: Subst): Subst =
    val s1 = this
    Subst(s2.subMap.mapValues(s1.apply(_)).toMap ++ s1.subMap)

object Subst:
  def apply(sub: (Type, TBasic)): Subst = Subst(Map(sub))
  def id: Subst = Subst(Map.empty)
