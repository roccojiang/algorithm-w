enum Expr:
  case EVar(x: String)
  case EAbs(x: EVar, e: Expr)
  case EApp(e1: Expr, e2: Expr)

  override def toString(): String = this match
    case EVar(x)      => x
    case EAbs(x, e)   => s"(λ$x.$e)"
    case EApp(e1, e2) => s"($e1$e2)"
