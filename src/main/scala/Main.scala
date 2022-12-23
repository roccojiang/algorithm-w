package ml

import scala.language.implicitConversions

import core.given
import core.Expr.*
import core.TermConst.*
import Inference.*

@main def playground: Unit =
  val I = EAbs("x", "x")
  val K = EAbs("x", EAbs("y", "x"))
  val S = EAbs("x", EAbs("y", EAbs("z", EApp(EApp("x", "z"), EApp("y", "z")))))

  val SK = EApp(S, K)
  val SKI = EApp(EApp(S, K), I)

  val p = EAbs("x", EApp("f", EApp("x", "x")))
  val Y = EAbs("f", EApp(p, p))

  println(s"$S: ${infer(S)}")
  println(s"$SK: ${infer(SK)}")
  println(s"$SKI: ${infer(SKI)}")
  println(s"$Y: ${infer(Y)}")

  val ii = ELet("i", I, EApp("i", "i"))
  println(s"$ii: ${infer(ii)}")

  val R = EFix(
    "r",
    EAbs("a", EAbs("b", EApp(EApp("r", EApp(EApp("r", "b"), K)), "a")))
  ) // TODO: test violating barendregts?
  println(s"$R: ${infer(R)}")

  // fix "t". λnm. Cond (IsZero n) 0 (Add ("t" (MinusOne n) m) m)
  val nIsZero = EApp(EApp(CEq, "n"), 0)
  val nMinusOne = EApp(EApp(CSub, "n"), 1)
  val tNMinusOneM = EApp(EApp("t", nMinusOne), "m")
  val addMOnce = EApp(EApp(CAdd, tNMinusOneM), "m")
  val conditional = EApp(EApp(EApp(CCond, nIsZero), 0), addMOnce)
  val times = EFix("t", EAbs("n", EAbs("m", conditional)))
  println(s"$times: ${infer(times)}")
