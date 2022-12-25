package parsing

import parsley.Parsley
import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc}
import parsley.token.{Lexer, predicate}

import ml.ast.TermConst

object lexer:
  private val boolKeywords = Set("True", "False")
  private val constKeywords = TermConst.constIds.keySet

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(_.isLetter),
      identifierLetter = predicate.Basic(_.isLetter)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set("let", "in", "fix") ++ boolKeywords ++ constKeywords
    )
  )

  private val lexer = Lexer(desc)

  val VAR = lexer.lexeme.names.identifier
  val INT = lexer.lexeme.numeric.integer.decimal
  val CHAR = lexer.lexeme.text.character.basicMultilingualPlane
  val BOOL = lexer.lexeme.symbol("True") #> true
    <|> lexer.lexeme.symbol("False") #> false

  val CONST = constKeywords
    .map(kw => lexer.lexeme.symbol(kw) #> TermConst.constIds(kw))
    .reduce(_ <|> _)

  val implicits = lexer.lexeme.symbol.implicits

  def parens[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.enclosing.parens(p)
