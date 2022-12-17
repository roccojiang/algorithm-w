enum Type:
  case TVar(x: Int)
  case TArr(a: Type, b: Type)

  def occurs(phi: TVar): Boolean = this match
    case TVar(x) => TVar(x) == phi
    case TArr(a, b) => a.occurs(phi) || b.occurs(phi)

  override def toString(): String = this match
    case TVar(x) => s"φ$x"
    case a TArr b => s"($a -> $b)"
