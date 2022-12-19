import core._
import core.TBasic._
import core.Expr._

class Inference:
  private var n: Int = 0

  def fresh: TVar =
    n += 1
    TVar(n)

object Inference:
  def unify(t1: Type, t2: Type): Either[String, Subst] =
    (t1, t2) match
      case (phi @ TVar(x), TVar(y)) if x == y => Right(Subst(phi -> phi))
      // TODO: does b have to be a basic type?
      case (phi @ TVar(x), b: TBasic) if !b.occurs(phi) =>
        Right(Subst(phi -> b))
      case (a, phi @ TVar(x)) => unify(phi, a)
      case (TArr(a, b), TArr(c, d)) =>
        for
          s1 <- unify(a, c)
          s2 <- unify(s1(b), s1(d))
        yield s2 compose s1
      case _ => Left(s"Unification failed on $t1 and $t2")

  def unifyContexts(c1: Context, c2: Context): Either[String, Subst] =
    if c1.isEmpty then Right(Subst.id)
    else
      val (x -> a) = c1.head
      if c2.contains(x) then
        for
          s1 <- unify(a, c2(x))
          s2 <- unifyContexts(s1(c1.tail), s1(c2.tail))
        yield s2 compose s1
      else unifyContexts(c1.tail, c2)

  def algW(context: Context, e: Expr): Either[String, (Subst, TBasic)] =

    def instantiate(t: Type)(using i: Inference): TBasic = t match
      case TPoly(quantified, a) =>
        val s = Subst(quantified.map(_ -> i.fresh).toMap)
        s(a)
      case t: TBasic => t

    def algWHelper(context: Context, e: Expr)(using
        i: Inference
    ): Either[String, (Subst, TBasic)] =
      e match
        case x: EVar =>
          if context.contains(x) then Right(Subst.id, instantiate(context(x)))
          else Left(s"$x not bound in $context")

        case EAbs(x, e) =>
          val phi = i.fresh
          for
            pp <- algWHelper(context ++ Map(x -> phi), e)
            (s, a) = pp
          yield (s, s(TArr(phi, a)))

        case EApp(e1, e2) => ???

    given Inference = new Inference()
    algWHelper(context, e)

  def infer(e: Expr): Either[String, (Subst, TBasic)] = algW(Map.empty, e)

  // def pp(e: Expr): Either[String, (Context, TBasic)] =

  //   def ppHelper(e: Expr)(using
  //       inferenceAlg: Inference
  //   ): Either[String, (Context, TBasic)] = e match
  //     case v @ EVar(x) =>
  //       val phi = inferenceAlg.fresh()
  //       Right(Map(v -> phi), phi)

  //     case EAbs(x, e) =>
  //       val phi = inferenceAlg.fresh()

  //       ppHelper(e).map((pi, p) =>
  //         if pi.contains(x) then
  //           val a = pi(x)
  //           (pi.removed(x), TArr(a, p))
  //         else (pi, TArr(phi, p))
  //       )
  //     // for
  //     //   pp <- ppHelper(e)
  //     //   (pi, p) = pp

  //     //   test =
  //     //     if pi.contains(x) then (pi.removed(x), TArr(pi(x), p))
  //     //     else (pi, TArr(phi, p))
  //     // yield test

  //     case EApp(e1, e2) =>
  //       val phi = inferenceAlg.fresh()

  //       for
  //         pp1 <- ppHelper(e1)
  //         pp2 <- ppHelper(e2)
  //         (pi1, p1) = pp1
  //         (pi2, p2) = pp2

  //         s1 <- unify(p1, TArr(p2, phi))
  //         s2 <- unifyContexts(s1(pi1), s1(pi2))
  //       yield (s2 compose s1)(pi1 ++ pi2, phi)

  //   given Inference = new Inference()
  //   ppHelper(e)
