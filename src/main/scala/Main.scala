package ml

import scala.language.implicitConversions

import ast.{given, *}
import ast.TermConst.*
import inference.Inference.*
import parsing.parser.*

@main def playground(): Unit =
  val I = EAbs("x", "x")
  val K = EAbs("x", EAbs("y", "x"))
  val S = EAbs("x", EAbs("y", EAbs("z", EApp(EApp("x", "z"), EApp("y", "z")))))

  val SK = EApp(S, K)
  val SKI = EApp(EApp(S, K), I)

  val p = EAbs("x", EApp("f", EApp("x", "x")))
  val Y = EAbs("f", EApp(p, p))

  // println(s"$S: ${infer(S)}")
  // println(s"$SK: ${infer(SK)}")
  // println(s"$SKI: ${infer(SKI)}")
  // println(s"$Y: ${infer(Y)}")

  val ii = ELet("i", I, EApp("i", "i"))
  println(s"$ii: ${infer(ii)}")

  val R = EFix(
    "r",
    EAbs("a", EAbs("b", EApp(EApp("r", EApp(EApp("r", "b"), K)), "a")))
  ) // TODO: test violating barendregts?
  // println(s"$R: ${infer(R)}")

  // fix t. λnm. Cond (IsZero n) 0 (Add ("t" (MinusOne n) m) m)
  val nIsZero = EApp(EApp(CEq, "n"), 0)
  val nMinusOne = EApp(EApp(CSub, "n"), 1)
  val tNMinusOneM = EApp(EApp("t", nMinusOne), "m")
  val addMOnce = EApp(EApp(CAdd, tNMinusOneM), "m")
  val conditional = EApp(EApp(EApp(CCond, nIsZero), 0), addMOnce)
  val times = EFix("t", EAbs("n", EAbs("m", conditional)))
  // println(s"$times: ${infer(times)}")

  println(expr.parse("(\\x.\\y.\\z.x z (y z))(\\a. \\b. a)"))
  println(expr.parse("(\\u.\\v.u v) (\\c.c) (\\y.\\z.z)"))
  println(expr.parse("a b c"))
  println(expr.parse("let i = \\x.x in i i"))
  val rParsed = expr.parse("fix r. \\a.\\b.r(r b(\\x.\\y.x))a")
  assert(rParsed.get == R)
  println(s"${rParsed.get}: ${infer(rParsed.get)}")
