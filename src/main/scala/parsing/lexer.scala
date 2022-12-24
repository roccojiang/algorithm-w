package parsing

import parsley.Parsley
import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc}
import parsley.token.{Lexer, predicate}

object lexer:
  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(_.isLetter),
      identifierLetter = predicate.Basic(_.isLetter)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set("let", "in", "fix")
    )
  )

  private val lexer = Lexer(desc)

  val VAR = lexer.lexeme.names.identifier

  val implicits = lexer.lexeme.symbol.implicits

  def parens[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.enclosing.parens(p)
