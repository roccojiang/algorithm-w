package parsing

import parsley.Parsley
import parsley.Parsley.pure
import parsley.implicits.character.charLift
import parsley.expr.chain

import lexer.{parens, VAR}
import ml.ast.Expr
import ml.ast.Expr.*

object parser:
  private val variable: Parsley[EVar] = EVar(VAR)
  private lazy val nonAppExpr: Parsley[Expr] =
    variable
      <|> EAbs('\\' ~> variable <~> '.' ~> expr)
      <|> parens(expr)
  lazy val expr: Parsley[Expr] = chain.left1(nonAppExpr, pure(EApp(_, _)))
