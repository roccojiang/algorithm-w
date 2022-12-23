import core.*
import core.BasicType.*
import core.Expr.*
import core.TermConst.*
import Inference.*

@main def playground: Unit =
  val f: EVar = EVar("f")
  val t: EVar = EVar("t")
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
  // println(s"$R: ${infer(R)}")

  // fix t. λnm. Cond (IsZero n) 0 (Add (t (MinusOne n) m) m)
  val n: EVar = EVar("n")
  val m: EVar = EVar("m")
  val nIsZero = EApp(EApp(EConst(CEq), n), EConst(CInt(0)))
  val nMinusOne = EApp(EApp(EConst(CSub), n), EConst(CInt(1)))
  val tNMinusOneM = EApp(EApp(t, nMinusOne), m)
  val addMOnce = EApp(EApp(EConst(CAdd), tNMinusOneM), m)
  val conditional = EApp(EApp(EApp(EConst(CCond), nIsZero), EConst(CInt(0))), addMOnce)
  val times = EFix(t, EAbs(n, EAbs(m, conditional)))
  println(s"$times: ${infer(times)}")
