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
      case TPoly(vars, a) =>
        val s = Subst(vars.map(_ -> i.fresh).toMap)
        s(a)
      case t: TBasic => t

    def generalise(context: Context, t: TBasic)(using i: Inference): TPoly =
      val vars = t.fv diff context.values.collect { case phi: TVar =>
        phi
      }.toSet
      TPoly(vars, t)
    
    def debugPrint(subst: Subst, t: TBasic): (Subst, TBasic) =
      println(s"  subst $subst; type $t")
      (subst, t)

    def algWHelper(context: Context, e: Expr)(using
        i: Inference
    ): Either[String, (Subst, TBasic)] =
      e match
        case x: EVar =>
          println(s"evar $x; $context; ")
          if context.contains(x) then Right(debugPrint(Subst.id, instantiate(context(x))))
          else Left(s"$x not bound in $context")

        case EAbs(x, e) =>
          println(s"eabs ${EAbs(x,e)}; $context; ")
          val phi = i.fresh
          for
            pp <- algWHelper(context ++ Map(x -> phi), e)
            (s, a) = pp
          yield debugPrint(s, s(TArr(phi, a)))

        case EApp(e1, e2) =>
          println(s"eapp ${EApp(e1,e2)}; $context; ")
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
          yield debugPrint(s3 compose s2 compose s1, s3(phi))

        case EFix(g, e) =>
          println(s"efix ${EFix(g,e)}; $context; ")
          val phi = i.fresh
          for
            pp <- algWHelper(context ++ Map(g -> phi), e)
            (s1, a) = pp
            s2 <- unify(s1(phi), a)
          yield debugPrint(s2 compose s1, s2(a))

        case ELet(x, e1, e2) =>
          println(s"elet ${ELet(x,e1,e2)}; $context; ")
          for
            pp1 <- algWHelper(context, e1)
            (s1, a) = pp1
            sigma = generalise(s1(context), a)
            pp2 <- algWHelper(s1(context ++ Map(x -> sigma)), e2)
            (s2, b) = pp2
          yield debugPrint(s2 compose s1, b)

    given Inference = new Inference()
    algWHelper(context, e)

  def infer(e: Expr): Either[String, TBasic] =
    for
      pp <- algW(Map.empty, e)
      (s, a) = pp
    yield s(a)
