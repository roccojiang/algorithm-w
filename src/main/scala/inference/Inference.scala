package ml.inference

import BasicType.*
import ml.ast.*

/** A substitution-type pair returned by Algorithm W. The final inferred type is
  * obtained by applying the substitution to the type.
  */
type WPair = (Subst, BasicType)

/** Errors are encoded as strings (for now). */
type Result[T] = Either[String, T]

class Inference:
  private var n: Int = 0

  def fresh: BasicType =
    n += 1
    TVar(n)

object Inference:
  def unify(t1: BasicType, t2: BasicType): Result[Subst] = (t1, t2) match
    case (phi @ TVar(x), TVar(y)) if x == y => Right(phi -> phi)

    case (TConst(c1), TConst(c2)) if c1 == c2 => Right(Subst.id)

    // Also covers the case unify φ c = (φ -> c)
    case (phi @ TVar(x), b) =>
      if !b.contains(phi) then Right(phi -> b)
      else Left(s"Cannot construct infinite type $t1 = $t2")

    case (a, phi @ TVar(x)) => unify(phi, a)

    case (TFun(a, b), TFun(c, d)) =>
      for
        s1 <- unify(a, c)
        s2 <- unify(s1(b), s1(d))
      yield s2 compose s1

    case _ => Left(s"Unification failed on $t1 and $t2")

  def algW(context: Context, e: Expr): Result[WPair] =

    def instantiate(t: PolyType)(using i: Inference): BasicType =
      val s = Subst(t.vars.map(_ -> i.fresh).toMap)
      s(t.a)

    def generalise(c: Context, t: BasicType)(using i: Inference): PolyType =
      val vars = t.fv diff c.fv
      PolyType(vars, t)

    def debugPrint(subst: Subst, t: BasicType): WPair =
      println(s"  subst $subst; type $t")
      (subst, t)

    def run(c: Context, e: Expr)(using i: Inference): Result[WPair] = e match
      case x: EVar =>
        if c.contains(x) then Right(Subst.id, instantiate(c(x)))
        else Left(s"$x not bound in $c")

      case EConst(c) => Right(Subst.id, instantiate(c.constType))

      case EAbs(x, e) =>
        val phi = i.fresh
        for (s, a) <- run(c ++ Map(x -> PolyType(phi)), e)
        yield (s, s(TFun(phi, a)))

      case EApp(e1, e2) =>
        val phi = i.fresh
        for
          (s1, a) <- run(c, e1)
          (s2, b) <- run(s1(c), e2)
          s3 <- unify(s2(a), TFun(b, phi))
        yield (s3 compose s2 compose s1, s3(phi))

      case EFix(g, e) =>
        val phi = i.fresh
        for
          (s1, a) <- run(c ++ Map(g -> PolyType(phi)), e)
          s2 <- unify(s1(phi), a)
        yield (s2 compose s1, s2(a))

      case ELet(x, e1, e2) =>
        for
          (s1, a) <- run(c, e1)
          sigma = generalise(s1(c), a)
          (s2, b) <- run(s1(c ++ Map(x -> sigma)), e2)
        yield (s2 compose s1, b)

    given Inference = new Inference() // new set of fresh variables
    run(context, e)

  def infer(e: Expr): Result[BasicType] =
    for (s, a) <- algW(Map.empty, e)
    yield s(a)
