package js7.base.parser

import cats.Show
import cats.data.NonEmptyList
import cats.parse.*
import cats.parse.Parser.{Error, Expectation, end}
import cats.syntax.show.*
import js7.base.parser.BasicPrinter.isIdentifierStart
import js7.base.parser.BasicParsers.w
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
//import js7.base.value.ValuePrinter.quoteString
import scala.util.control.NonFatal

object Parsers:

  object syntax:
    implicit final class RichParser0[+A](private val parser0: Parser0[A]) extends AnyVal:
      //def ignore: IgnoredParser0 =
      //  IgnoredParser0(parser0.void)

      def ~~[B](other: Parser0[B]): Parser0[(A, B)] =
        parser0 ~ (w *> other)

      //def ~>(ignore: IgnoredParser): Parser[A] =
      //  parser0.with1 <* ignore.parser
      //
      //def ~>(ignore: IgnoredParser0): Parser0[A] =
      //  parser0 <* ignore.parser0

      def ~~[B](other: Parser[B]): Parser[(A, B)] =
        (parser0 <* w).with1 ~ other

      //def ~~>(ignore: IgnoredParser0): Parser0[A] =
      //  parser0 <* w <* ignore.parser0
      //
      //def ~~>(ignore: IgnoredParser): Parser0[A] =
      //  parser0 <* w <* ignore.parser

      def ~*>[B](other: Parser0[B]): Parser0[B] =
        parser0 *> other

      def ~*>[B](other: Parser[B]): Parser[B] =
        parser0.with1 *> other

      def ~~*>[B](other: Parser0[B]): Parser0[B] =
        (parser0 ~ w) *> other

      def ~~*>[B](other: Parser[B]): Parser[B] =
        (parser0 ~ w).with1 *> other

      def ~<*[B](skip: Parser0[B]): Parser0[A] =
        parser0 <* skip

      def ~<*[B](skip: Parser[B]): Parser[A] =
        parser0.with1 <* skip

      def ~~<*[B](skip: Parser0[B]): Parser0[A] =
        parser0 <* (w ~ skip)

      def ~~<*[B](skip: Parser[B]): Parser[A] =
        parser0.with1 <* (w.with1 ~ skip)

    implicit final class RichParser[+A](private val parser: Parser[A]) extends AnyVal:
      //def ignore: IgnoredParser =
      //  IgnoredParser(parser.void)
      //
      //def ignore1: IgnoredParser =
      //  IgnoredParser(parser.void)
      //
      //def ~(ignore: IgnoredParser0): Parser[A] =
      //  parser <* ignore.parser0
      //
      //def ~>(ignore: IgnoredParser): Parser0[A] =
      //  parser <* ignore.parser
      //
      //def ~>(ignore: IgnoredParser0): Parser[A] =
      //  parser <* ignore.parser0

      def ~~[B](other: Parser0[B]): Parser[(A, B)] =
        parser ~ (w *> other)

      //def ~~(ignore: IgnoredParser0): Parser[A] =
      //  parser <* w <* ignore.parser0

      def ~*>[B](other: Parser0[B]): Parser0[B] =
        parser *> other

      def ~*>[B](other: Parser[B]): Parser[B] =
        parser *> other

      def ~<*[B](skip: Parser0[B]): Parser[A] =
        parser <* skip

      def ~~<*[B](skip: Parser0[B]): Parser[A] =
        parser <* (w ~ skip)

  /** Parses the whole string, return a `Checked`. */
  def checkedParse[A](string: String, parser: Parser0[A]): Checked[A] =
    (parser <* end).parse(string) match
      case Right(("", expr)) => Right(expr)
      case Right((_, _)) => Left(Problem.pure("Incompletely parsed"))
      case Left(error) => Left(ParsingProblem(string, error))

  private val ErrorStart = "Parsing failed at position "

  implicit val ErrorShow: Show[Parser.Error] = error =>
    ErrorStart + (error.failedAtOffset + 1) + expectationsToCommaString(error.expected)

  implicit val ExpectationShow: Show[Expectation] =
    case Expectation.OneOfStr(_, strings) =>
      strings.distinct.map(o => s"“$o”") match
        case o :: Nil => s"Expected $o"
        case list => "Expected one of " + list.mkString("\"", ", ", "\"")
    case Expectation.InRange(_, lower, upper) =>
      if lower == upper then s"expected '$lower'"
      else s"Expected '$lower'-'$upper'"
    case Expectation.StartOfString(_) =>
      "Expected start of input"
    case Expectation.EndOfString(_, _) =>
      "Expected end of input"
    case Expectation.Length(_, expected, actual) =>
      "Unexpected end of input"
    case Expectation.ExpectedFailureAt(_, matched) =>
      s"Unexpected “$matched”"
    case Expectation.Fail(_) =>
      "Failed to parse"
    case Expectation.FailWith(_, message) =>
      message
    case Expectation.WithContext(context, _) =>
      s"Expected $context"

  private def errorToString(source: String, error: Parser.Error): String =
    val locationString = try
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
    catch { case NonFatal(t) => " " }
    ErrorStart + locationString + expectationsToCommaString(error.expected)

  private def expectationsToCommaString(expectations: NonEmptyList[Expectation]): String =
    val s = expectationsToString(expectations)
    (s.nonEmpty ?? " · ") + s

  private val AllChars = Char.MinValue to Char.MaxValue

  private def expectationsToString(expectations: NonEmptyList[Expectation]): String =
    // Reduce the huge amount of Expectation.InRange
    var charSetExpectations: List[String] = Nil
    var expectedChars = expectations
      .toList.view
      .collect { case Expectation.InRange(_, a, b) => a to b }
      .flatten
      .toSet

    if expectedChars.size > AllChars.size - 20 then
      val notIn = AllChars.filterNot(expectedChars).sorted
      val notInAsString = charsToString(notIn)
      val msg = notIn.size match
        case 0 =>
          "Expected any character"
        case 1 =>
          val s = notInAsString.replace("'", "\\'")
          s"Expected character != '$s'"
        case _ =>
          val s = notInAsString.replace("]", "\\]")
          s"Expected character not in [$s]"
      charSetExpectations = msg :: charSetExpectations
      expectedChars = Set.empty

    if AllChars.filter(isIdentifierStart) forall expectedChars then
      charSetExpectations = "Expected identifer" :: charSetExpectations
      expectedChars = expectedChars filterNot isIdentifierStart

    expectations.toList
      .filterNot(_.isInstanceOf[Expectation.InRange])
      .map(_.show)
      .concat(charSetExpectations)
      .concat:
        val expectedCharsAsString = charsToString(expectedChars)
        expectedChars.size match
          case 0 => None
          case 1 =>
            val s = expectedCharsAsString.replace("'", "\\'")
            Some(s"Expected character '$s'")
          case _ =>
            val s = expectedCharsAsString.replace("]", "\\]")
            Some(s"Expected a character out of [$s]")
      .mkString(" · ")

  private def charsToString(chars: Iterable[Char]): String =
    chars.toVector.sortBy(c => (c.toInt & Int.MaxValue)).view.flatMap {
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

  //final case class IgnoredParser0(parser0: Parser0[Unit]) {
  //  def ~[B](o: Parser0[B]): Parser0[B] =
  //    parser0 *> o
  //
  //  def ~(o: IgnoredParser0): IgnoredParser0 =
  //    IgnoredParser0((parser0 ~ o.parser0).void)
  //
  //  def ~(o: IgnoredParser): IgnoredParser =
  //    IgnoredParser((parser0.with1 ~ o.parser).void)
  //
  //  def ~[B](o: Parser[B]): Parser[B] =
  //    parser0.with1 *> o
  //
  //  def ~~[B](other: Parser0[B]): Parser0[B] =
  //    parser0 *> w *> other
  //
  //  def ~~[B](other: Parser[B]): Parser[B] =
  //    (parser0 *> w).with1 *> other
  //}
  //
  //final case class IgnoredParser(parser: Parser[Unit]) {
  //  def ~[B](o: Parser0[B]): Parser0[B] =
  //    parser *> o
  //
  //  def ~[B](o: Parser[B]): Parser[B] =
  //    parser.with1 *> o
  //
  //  def ~(o: IgnoredParser0): IgnoredParser =
  //    IgnoredParser((parser ~ o.parser0).void)
  //
  //  def ~(o: IgnoredParser): IgnoredParser =
  //    IgnoredParser((parser ~ o.parser).void)
  //
  //  def ~~[B](other: Parser0[B]): Parser0[B] =
  //    parser *> w *> other
  //
  //  def ~~[B](other: Parser[B]): Parser[B] =
  //    (parser *> w).with1 *> other
  //
  //  def ~~(ignored: IgnoredParser0): IgnoredParser0 =
  //    IgnoredParser0((parser ~~ ignored.parser0).void)
  //
  //  def ~~(ignored: IgnoredParser): IgnoredParser =
  //    IgnoredParser((parser ~~ ignored.parser).void)
  //}

  final case class ParsingProblem(source: String, error: Error)
  extends Problem.Lazy(errorToString(source, error))
