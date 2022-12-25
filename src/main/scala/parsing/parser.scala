package parsing

import parsley.Parsley
import parsley.Parsley.pure
import parsley.combinator.some
import parsley.expr.chain

import lexer.*
import lexer.implicits.implicitSymbol
import ml.ast.*

object parser:
  private val variable = EVar(VAR)
  private val constExpr = EConst(
    CInt(INT) <|> CChar(CHAR) <|> CBool(BOOL) <|> CONST
  )
  private lazy val exprAtom =
    variable
      <|> EAbs("\\" ~> some(variable), "." ~> expr)
      <|> ELet("let" ~> variable, "=" ~> expr, "in" ~> expr)
      <|> EFix("fix" ~> variable, "." ~> expr)
      <|> constExpr
      <|> parens(expr)

  lazy val expr: Parsley[Expr] = chain.left1(exprAtom, pure(EApp.apply))
