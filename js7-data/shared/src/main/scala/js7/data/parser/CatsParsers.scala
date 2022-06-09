package js7.data.parser

import cats.Show
import cats.data.NonEmptyList
import cats.parse.Parser.{Error, Expectation, end}
import cats.parse._
import cats.syntax.show._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.parser.BasicPrinter.isIdentifierStart
import js7.data.value.ValuePrinter.quoteString
import scala.util.control.NonFatal

object CatsParsers
{
  /** Parses the whole string, return a `Checked`. */
  def checkedParse[A](string: String, parser: Parser0[A]): Checked[A] =
    (parser <* end).parse(string) match {
      case Right(("", expr)) => Right(expr)
      case Right((_, _)) => Left(Problem.pure("Incompletely parsed"))
      case Left(error) => Left(ParsingProblem(string, error))
    }

  private val ErrorStart = "Parsing failed at position "

  implicit val ErrorShow: Show[Parser.Error] = error =>
    ErrorStart + (error.failedAtOffset + 1) + expectationsToCommaString(error.expected)

  implicit val ExpectationShow: Show[Expectation] = {
    case Expectation.OneOfStr(_, strings) =>
      strings.distinct.map(quoteString) match {
        case o :: Nil => s"Expected $o"
        case list => "Expected one of " + list.mkString(", ")
      }
    case Expectation.InRange(_, lower, upper) =>
      if (lower == upper) s"expected '$lower'"
      else s"Expected '$lower'-'$upper'"
    case Expectation.StartOfString(_) =>
      "Expected start of input"
    case Expectation.EndOfString(_, _) =>
      "Expected end of input"
    case Expectation.Length(_, expected, actual) =>
      s"Unexpected end of input"
    case Expectation.ExpectedFailureAt(_, matched) =>
      "Unexpected " + quoteString(matched)
    case Expectation.Fail(_) =>
      "Failed to parse"
    case Expectation.FailWith(_, message) =>
      message
    case Expectation.WithContext(context, _) =>
      s"Expected $context"
  }

  private def errorToString(source: String, error: Parser.Error): String = {
    val locationString = try {
      val locationMap = LocationMap(source)
      val (row0, col0) = locationMap.toLineCol(error.failedAtOffset)
        .getOrElse(
          locationMap.lineCount ->
            locationMap.getLine(locationMap.lineCount - 1).fold(0)(_.length))
      val before = 10 min col0
      val after = 10
      val longContext = locationMap.getLine(row0).getOrElse("").drop(col0 - before)
      val context =
        ((before < col0) ?? "…") +
        longContext.take(before) + "❓" +
        longContext.slice(before, before + after).takeWhile(_ >= ' ') +
        ((longContext.length > before + after) ?? "…")
      ((locationMap.lineCount > 1) ?? s"${row0 + 1}:") +
        (col0 + 1) +
        " “" + context + "”" //" »" + context + "«"
    } catch { case NonFatal(t) => " " }
    ErrorStart + locationString + expectationsToCommaString(error.expected)
  }

  private def expectationsToCommaString(expectations: NonEmptyList[Expectation]): String = {
    val s = expectationsToString(expectations)
    (s.nonEmpty ?? " · ") + s
  }

  private val AllChars = Char.MinValue to Char.MaxValue

  private def expectationsToString(expectations: NonEmptyList[Expectation]): String = {
    // Reduce the huge amount of Expectation.InRange
    var charSetExpectations: List[String] = Nil
    var expectedChars = expectations
      .toList.view
      .collect { case Expectation.InRange(_, a, b) => a to b }
      .flatten
      .toSet

    if (expectedChars.size > AllChars.size - 20) {
      val notIn = AllChars.filterNot(expectedChars).sorted
      val notInAsString = charsToString(notIn)
      val msg = notIn.size match {
        case 0 =>
          "Expected any character"
        case 1 =>
          val s = notInAsString.replace("'", "\\'")
          s"Expected character != '$s'"
        case _ =>
          val s = notInAsString.replace("]", "\\]")
          s"Expected character not in [$s]"
      }
      charSetExpectations = msg :: charSetExpectations
      expectedChars = Set.empty
    }

    if (AllChars.filter(isIdentifierStart) forall expectedChars) {
      charSetExpectations = "Expected identifer" :: charSetExpectations
      expectedChars = expectedChars filterNot isIdentifierStart
    }

    expectations.toList
      .filterNot(_.isInstanceOf[Expectation.InRange])
      .map(_.show)
      .concat(charSetExpectations)
      .concat {
        val expectedCharsAsString = charsToString(expectedChars)
        expectedChars.size match {
          case 0 => None
          case 1 =>
            val s = expectedCharsAsString.replace("'", "\\'")
            Some(s"Expected character '$s'")
          case _ =>
            val s = expectedCharsAsString.replace("]", "\\]")
            Some(s"Expected character out of [$s]")
        }
      }
      .mkString(" · ")
  }

  private def charsToString(chars: Iterable[Char]): String =
    chars.view.flatMap {
      case '\\' => "\\\\"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case '\n' => "\\n"
      case char => char.toString
    }.mkString

  //private def expectationToString(locationMap: LocationMap, expectation: Expectation): String = {
  //  val (row0, col0) = locationMap.toLineCol(expectation.offset)
  //    .getOrElse(
  //      locationMap.lineCount ->
  //        locationMap.getLine(locationMap.lineCount - 1).fold(0)(_.length))
  //  val remainder = locationMap.getLine(row0).getOrElse("").slice(col0, col0 + 10)
  //  val row = if (locationMap.lineCount <= 1) "" else s"${row0 + 1}:"
  //  row + (col0 + 1) + ": " + expectation.show + ": " + remainder
  //}

  final case class ParsingProblem(source: String, error: Error)
  extends Problem.Lazy(errorToString(source, error))
}