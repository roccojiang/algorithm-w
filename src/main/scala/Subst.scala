import Expr._
import Type._
import scala.collection.immutable

type Context = Map[EVar, Type]

case class Subst(subMap: Map[Type, Type]):
  def apply(t: Type): Type = t match
    case phi @ TVar(x) => subMap.getOrElse(phi, phi)
    case a TArr b      => TArr(apply(a), apply(b))

  def apply(c: Context): Context = c.map((x, a) => x -> apply(a))

  def apply(pp: (Context, Type)): (Context, Type) =
    val (c, t) = pp
    (apply(c), apply(t))

  def compose(s: Subst): Subst = Subst(subMap ++ s.subMap)

object Subst:

  def apply(sub: (Type, Type)): Subst = Subst(Map(sub))
  def id: Subst = Subst(Map.empty)
