package ml.inference

import scala.collection.immutable

/** Type substitution, mapping type variables to (basic) types. */
case class Subst(subst: Map[BasicType, BasicType]):

  def apply[T: Types](t: T): T = t.subst(this)

  def compose(s2: Subst): Subst =
    val s1 = this // (s1 . s2): just for clarity
    Subst(s1.subst ++ s2.subst.view.mapValues(s1(_)).toMap)

  def getOrElse(key: BasicType, default: BasicType): BasicType =
    subst.getOrElse(key, default)

object Subst:
  def id: Subst = Subst(Map.empty)

given Conversion[(BasicType, BasicType), Subst] with
  def apply(pair: (BasicType, BasicType)): Subst = Subst(Map(pair))
