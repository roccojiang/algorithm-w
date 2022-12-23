import core.*
import core.BasicType.*
import core.Expr.*
import core.TermConst.*
import Inference.*

@main def playground: Unit =
  val f: EVar = EVar("f")
  val a: EVar = EVar("a")
  val b: EVar = EVar("b")
  val x: EVar = EVar("x")
  val y: EVar = EVar("y")
  val z: EVar = EVar("z")

  val I = EAbs(x, x)
  val K = EAbs(x, EAbs(y, x))
  val S = EAbs(x, EAbs(y, EAbs(z, EApp(EApp(x, z), EApp(y, z)))))

  val SK = EApp(S, K)
  val SKI = EApp(EApp(S, K), I)

  val p = EAbs(x, EApp(f, EApp(x, x)))
  val Y = EAbs(f, EApp(p, p))

  // println(s"$S: ${infer(S)}")
  // println(s"$SK: ${infer(SK)}")
  // println(s"$SKI: ${infer(SKI)}")
  // println(s"$Y: ${infer(Y)}")

  val i: EVar = EVar("i")
  val ii = ELet(i, I, EApp(i, i))
  val r: EVar = EVar("r")
  val bar = EAbs(a, EAbs(b, EApp(EApp(r, EApp(EApp(r, b), K)), a)))
  val R = EFix(r, bar) // TODO: test violating barendregts?
  // println(s"$ii: ${infer(ii)}")
  println(s"$R: ${infer(R)}") // TODO: incorrect

  val baz = EAbs(x, EConst(CBool(true)))
  println(s"$baz: ${infer(baz)}")

  // test pretty printing of expressions
  // println(ELet(EVar("foo"), EFix(f, Y), ELet(EVar("bar"), S, EFix(f, ELet(EVar("baz"), x, x)))))
  // println(EApp(x, ELet(EVar("foo"), x, x)))
  // println(EApp(x, EFix(f, p)))

  // val p1: TVar = TVar(1)
  // val p2: TVar = TVar(2)
  // val test = pp(SKI)
  // println(TPoly(Set(p1, p2), pp(SKI).getOrElse(null)._2))

  // testing new substitutions
  val p1: TVar = TVar(1)
  val p2 = TVar(2)
  val tPoly: Type = PolyType(Set(p1), p1)
  val s = Subst(p1 -> p2)
  // println(s(p1))
  // println(s(tPoly))
