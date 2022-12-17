import Type._
import Expr._

class Inference:
  private var n: Int = 0

  def fresh(): TVar =
    n += 1
    TVar(n)

object Inference:
  def unify(t1: Type, t2: Type): Either[String, Subst] =
    // println(s"unify $t1 $t2")
    (t1, t2) match
      case (phi @ TVar(x), TVar(y)) if x == y   => Right(Subst(phi -> phi))
      case (phi @ TVar(x), b) if !b.occurs(phi) => Right(Subst(phi -> b))
      case (a, phi @ TVar(x))                   => unify(phi, a)
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

  def pp(e: Expr): Either[String, (Context, Type)] =

    def ppHelper(e: Expr)(using
        inferenceAlg: Inference
    ): Either[String, (Context, Type)] = e match
      case v @ EVar(x) =>
        val phi = inferenceAlg.fresh()
        // println(s"$v: $phi")
        Right(Map(v -> phi), phi)

      case EAbs(x, e) =>
        val phi = inferenceAlg.fresh()
        // println(s"${EAbs(x,e)}: $phi")

        ppHelper(e).map((pi, p) =>
          if pi.contains(x) then
            val a = pi(x)
            val pi_ = pi.removed(x)
            (pi_, TArr(a, p))
          else (pi, TArr(phi, p))
        )

      case EApp(e1, e2) =>
        val phi = inferenceAlg.fresh()
        // println(s"${EApp(e1,e2)}: $phi")

        for
          pp1 <- ppHelper(e1)
          pp2 <- ppHelper(e2)
          (pi1, p1) = pp1
          (pi2, p2) = pp2

          // _ = println(s"$e1: $pi1, $p1")
          // _ = println(s"$e2: $pi2, $p2")

          s1 <- unify(p1, TArr(p2, phi))
          s2 <- unifyContexts(s1(pi1), s1(pi2))

        // _ = println(s"s1: $s1")
        // _ = println(s"s2: $s2")
        // _ = println(s"pi1: $pi1, pi2: $pi2, union: ${pi1 ++ pi2}")
        // _ = println(s"s1($phi): ${s1(phi)}")
        // _ = println(s"s2.s1($phi): ${s2(s1(phi))}")
        // _ = println(s"($e1)($e2): ${(s2 compose s1)(pi1 ++ pi2, phi)}")
        yield (s2 compose s1)(pi1 ++ pi2, phi)

    given Inference = Inference()
    ppHelper(e)
