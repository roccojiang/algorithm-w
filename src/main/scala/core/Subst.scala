package ml.core

import scala.collection.immutable

case class Subst(subst: Map[BasicType, BasicType]):

  def apply[T: Types](t: T) = t.subst(this)

  def compose(s2: Subst): Subst =
    val s1 = this // (s1 . s2): just for clarity
    Subst(s1.subst ++ s2.subst.mapValues(s1(_)).toMap)

  def getOrElse(key: BasicType, default: BasicType) =
    subst.getOrElse(key, default)

object Subst:
  def id: Subst = Subst(Map.empty)

given Conversion[(BasicType, BasicType), Subst] with
  def apply(pair: (BasicType, BasicType)) = Subst(Map(pair))
