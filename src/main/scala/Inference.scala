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

        case EApp(e1, e2) =>
          val phi = i.fresh
          for
            pp1 <- algWHelper(context, e1)
            (s1, a) = pp1
            // _ = println(s"    pp $e1: $pp1")
            pp2 <- algWHelper(s1(context), e2)
            (s2, b) = pp2
            // _ = println(s"    pp $e2: $pp2")
            s3 <- unify(s2(a), TArr(b, phi))
            // _ = println(s"    pp ${EApp(e1,e2)}: ${(s3 compose s2 compose s1, s3(phi))}")
          yield (s3 compose s2 compose s1, s3(phi))

    given Inference = new Inference()
    algWHelper(context, e)

  def infer(e: Expr): Either[String, TBasic] =
    for
      pp <- algW(Map.empty, e)
      (s, a) = pp
    yield s(a)
