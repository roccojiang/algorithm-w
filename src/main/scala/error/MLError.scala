package ml.error

import scala.io.AnsiColor.*

sealed trait MLError

case class TypeError(msg: String) extends MLError:
  override def toString: String = msg

given Conversion[String, TypeError] = TypeError(_)

case class SyntaxError(col: Int, lines: SyntaxErrorLines) extends MLError:
  override def toString: String =
    s"""(column $col):
       |$lines
    """.stripMargin

enum SyntaxErrorItem:
  case EndOfInput
  case Raw(item: String)
  case Named(item: String)

  override def toString: String = this match
    case EndOfInput  => "end of input"
    case Raw(item)   => item
    case Named(item) => item

case class SyntaxErrorLineInfo(
    line: String,
    errorPointsAt: Int,
    errorWidth: Int
):
  private val errorLineStart = "> "
  private def errorPointer(caretAt: Int, caretWidth: Int) =
    s"${" " * caretAt}${"^" * caretWidth}"

  override def toString: String =
    s"""$errorLineStart$line
       |${" " * errorLineStart.length}${errorPointer(errorPointsAt, errorWidth)}
    """.stripMargin

sealed trait SyntaxErrorLines

case class VanillaError(
    unexpected: Option[SyntaxErrorItem],
    expected: Set[SyntaxErrorItem],
    reasons: Seq[String],
    line: SyntaxErrorLineInfo
) extends SyntaxErrorLines:
  override def toString =
    val unexpected_ = unexpected
      .map("unexpected " + _)
      .getOrElse("")
    val expecteds_ =
      "expected one of: " + expected.mkString(", ")
    val reasons_ =
      reasons.map("  " + _).mkString("\n")

    s"""$unexpected_
       |$expecteds_
       |${if reasons_.nonEmpty then s"$reasons_\n" else ""}
       |$line
    """.stripMargin

case class SpecialisedError(msgs: Seq[String], line: SyntaxErrorLineInfo)
    extends SyntaxErrorLines:
  override def toString() = s"${msgs.mkString("\n")}\n\n$line"
