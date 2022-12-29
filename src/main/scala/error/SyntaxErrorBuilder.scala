package ml.error

import parsley.errors.ErrorBuilder
import parsley.errors.Token
import parsley.errors.tokenextractors.MatchParserDemand

class SyntaxErrorBuilder
    extends ErrorBuilder[SyntaxError]
    with MatchParserDemand:

  override def format(pos: Position, source: Source, lines: ErrorInfoLines) =
    SyntaxError(pos, lines)

  override def vanillaError(
      unexpected: UnexpectedLine,
      expected: ExpectedLine,
      reasons: Messages,
      line: LineInfo
  ) = VanillaError(unexpected, expected, reasons, line)

  type Source = Unit
  override def source(sourceName: Option[String]) = ()

  type Position = Int
  override def pos(line: Int, col: Int) = col

  type Item = SyntaxErrorItem
  type EndOfInput = SyntaxErrorItem.EndOfInput.type
  type Raw = SyntaxErrorItem.Raw
  type Named = SyntaxErrorItem.Named

  override val endOfInput = SyntaxErrorItem.EndOfInput
  override def raw(item: String) = SyntaxErrorItem.Raw(item)
  override def named(item: String) = SyntaxErrorItem.Named(item)

  type LineInfo = SyntaxErrorLineInfo
  override def lineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int,
      errorWidth: Int
  ) = SyntaxErrorLineInfo(line, errorPointsAt, errorWidth)

  type ErrorInfoLines = SyntaxErrorLines

  override def specialisedError(msgs: Messages, line: LineInfo) =
    SpecialisedError(msgs, line)

  type Message = String
  override def reason(reason: String) = reason
  override def message(msg: String) = msg

  type Messages = Seq[String]
  override def combineMessages(alts: Seq[Message]) = alts

  override val numLinesBefore: Int = 0
  override val numLinesAfter: Int = 0

  type ExpectedItems = Set[Item]
  type ExpectedLine = ExpectedItems
  type UnexpectedLine = Option[Item]

  override def unexpected(item: Option[Item]) = item
  override def expected(alts: ExpectedItems) = alts
  override def combineExpectedItems(alts: Set[Item]) = alts
