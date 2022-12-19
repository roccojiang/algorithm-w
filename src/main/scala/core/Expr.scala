package core

enum Expr:
  case EVar(x: String)
  case EAbs(x: EVar, e: Expr)
  case EApp(e1: Expr, e2: Expr)

  override def toString: String =
    def exprToString(e: Expr): String = e match
      case EAbs(EVar(x), e) => s"λ$x${absExprToString(e)}"
      case e                => appExprToString(e)

    def absExprToString(e: Expr): String = e match
      case EAbs(EVar(x), e @ EAbs(_, _)) => s"$x${absExprToString(e)}"
      case EAbs(EVar(x), e)              => s"$x.${exprToString(e)}"
      case e                             => s".${exprToString(e)}"

    def appExprToString(e: Expr): String = e match
      case EApp(e1, e2) => s"${appExprToString(e1)}${varExprToString(e2)}"
      case e            => varExprToString(e)

    def varExprToString(e: Expr): String = e match
      case EVar(x) => x
      case e       => s"(${exprToString(e)})"

    exprToString(this)
