package ml

import core.{given, *}
import core.BasicType.*
import core.Expr.*

/** A substitution-type pair returned by Algorithm W. The final inferred type is
  * obtained by applying the substitution to the type.
  */
type WPair = (Subst, BasicType)

class Inference:
  private var n: Int = 0

  def fresh =
    n += 1
    TVar(n)

object Inference:
  def unify(t1: BasicType, t2: BasicType): Either[String, Subst] =
    (t1, t2) match
      case (phi @ TVar(x), TVar(y)) if x == y   => Right(phi -> phi)
      case (TConst(c1), TConst(c2)) if c1 == c2 => Right(Subst.id)
      // Below case covers the "unify phi c" case with TConst
      case (phi @ TVar(x), b) if !b.occurs(phi) => Right(phi -> b)
      case (a, phi @ TVar(x))                   => unify(phi, a)
      case (TFun(a, b), TFun(c, d)) =>
        for
          s1 <- unify(a, c)
          s2 <- unify(s1(b), s1(d))
        yield s2 compose s1
      case _ => Left(s"Unification failed on $t1 and $t2")

  def algW(context: Context, e: Expr): Either[String, WPair] =

    def instantiate(t: PolyType)(using i: Inference): BasicType =
      val s = Subst(t.vars.map(_ -> i.fresh).toMap)
      s(t.a)

    def generalise(context: Context, t: BasicType)(using
        i: Inference
    ): PolyType =
      val vars = t.fv diff context.fv
      PolyType(vars, t)

    def debugPrint(subst: Subst, t: BasicType): WPair =
      println(s"  subst $subst; type $t")
      (subst, t)

    def algWHelper(context: Context, e: Expr)(using
        i: Inference
    ): Either[String, WPair] =
      e match
        case x: EVar =>
          if context.contains(x) then Right(Subst.id, instantiate(context(x)))
          else Left(s"$x not bound in $context")
        
        case EConst(c) => Right(Subst.id, instantiate(c.constType))

        case EAbs(x, e) =>
          val phi = i.fresh
          for (s, a) <- algWHelper(context ++ Map(x -> PolyType(phi)), e)
          yield (s, s(TFun(phi, a)))

        case EApp(e1, e2) =>
          val phi = i.fresh
          for
            (s1, a) <- algWHelper(context, e1)
            (s2, b) <- algWHelper(s1(context), e2)
            s3 <- unify(s2(a), TFun(b, phi))
          yield (s3 compose s2 compose s1, s3(phi))

        case EFix(g, e) =>
          val phi = i.fresh
          for
            (s1, a) <- algWHelper(context ++ Map(g -> PolyType(phi)), e)
            s2 <- unify(s1(phi), a)
          yield (s2 compose s1, s2(a))

        case ELet(x, e1, e2) =>
          for
            (s1, a) <- algWHelper(context, e1)
            sigma = generalise(s1(context), a)
            (s2, b) <- algWHelper(s1(context ++ Map(x -> sigma)), e2)
          yield (s2 compose s1, b)

    given Inference = new Inference()
    algWHelper(context, e)

  def infer(e: Expr): Either[String, BasicType] =
    for (s, a) <- algW(Map.empty, e)
    yield s(a)
