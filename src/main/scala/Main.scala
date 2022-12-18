import core._
import core.BasicType._
import core.Expr._
import Inference._

@main def playground: Unit =
  val f: EVar = EVar("f")
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

  println(s"$S: ${pp(S)}")
  println(s"$SK: ${pp(SK)}")
  println(s"$SKI: ${pp(SKI)}")
  println(s"$Y: ${pp(Y)}")

  val p1: TVar = TVar(1)
  val p2: TVar = TVar(2)
  val test = pp(SKI)
  println(PolymorphicType(Set(p1, p2), pp(SKI).getOrElse(null)._2))
