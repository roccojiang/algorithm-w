package core

import BasicType.*
import TypeConst.*

enum Expr:
  case EVar(x: String)
  case EConst(c: TermConst)
  case EAbs(x: EVar, e: Expr)
  case EApp(e1: Expr, e2: Expr)
  case ELet(x: EVar, e1: Expr, e2: Expr)
  case EFix(g: EVar, e: Expr)

  // TODO: complete rework required:
  // add spacing (very relevant for term constants)
  // remove redundant parentheses on let and fix
  override def toString: String =
    // Abbreviates consecutive abstractions
    def absStr(e: Expr): String = e match
      case EAbs(EVar(x), e @ EAbs(_, _)) => s"$x${absStr(e)}"
      case EAbs(EVar(x), e)              => s"$x.$e"
      case e                             => s".$e"

    def fixStr(e: Expr): String = e match
      case EFix(g, e) => s"fix $g. $e"
      case e          => letStr(e)

    def letStr(e: Expr): String = e match
      case ELet(x, e1, e2) => s"let $x = ${appStr(e1)} in ${appStr(e2)}"
      case e               => appStr(e)

    def appStr(e: Expr): String = e match
      case EApp(e1, e2) => s"${appStr(e1)}${varStr(e2)}"
      case e            => varStr(e)

    def varStr(e: Expr): String = e match
      case EVar(x)   => x
      case EConst(c) => c.toString
      case e         => s"($e)"

    this match
      case EAbs(EVar(x), e) => s"λ$x${absStr(e)}"
      case e                => fixStr(e)

enum TermConst:
  case CInt(x: Int)
  case CChar(c: Char)
  case CBool(b: Boolean)

  case CCond
  case CEq
  case CAdd
  case CSub

  /** The function 'v', which maps each term constant to its (closed) type. */
  // TODO: implicits
  def constType: PolyType =
    val phi: TVar = TVar(0)

    this match
      case _: CInt  => PolyType(TConst(TInt))
      case _: CChar => PolyType(TConst(TChar))
      case _: CBool => PolyType(TConst(TBool))

      case CCond => // ∀φ. Bool -> φ -> φ -> φ
        PolyType(Set(phi), TFun(TConst(TBool), TFun(phi, TFun(phi, phi))))
      case CEq => // ∀φ. φ -> φ -> Bool
        PolyType(Set(phi), TFun(phi, TFun(phi, TConst(TBool))))
      case CAdd | CSub => // Int -> Int -> Int
        PolyType(TFun(TConst(TInt), TFun(TConst(TInt), TConst(TInt))))
        
  override def toString(): String = this match
    case CInt(x)  => x.toString
    case CChar(c) => s"'$c'"
    case CBool(b) => b.toString

    case CCond => "Cond"
    case CEq => "Eq"
    case CAdd => "Add"
    case CSub => "Sub"
