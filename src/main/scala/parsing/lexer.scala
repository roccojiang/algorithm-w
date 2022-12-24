package parsing

import parsley.Parsley
import parsley.token.descriptions.{LexicalDesc, NameDesc}
import parsley.token.{Lexer, predicate}

object lexer:
  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(_.isLetter),
      identifierLetter = predicate.Basic(_.isLetter)
    )
  )

  private val lexer = Lexer(desc)

  val VAR = lexer.lexeme.names.identifier

  def parens[A](p: =>Parsley[A]): Parsley[A] = lexer.lexeme.enclosing.parens(p)
