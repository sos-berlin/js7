package js7.base.utils

import cats.parse.Parser.{end, string}
import cats.parse.{Parser, Parser0}
import js7.base.parser.BasicParsers.{isWhiteChar, w}
import js7.base.parser.Parsers.checkedParse
import js7.base.problem.Checked
import js7.base.utils.RangeSet.{Interval, Single}
import js7.base.utils.RangeSetParser.*

private final class RangeSetParser[A: Ordering: Ordinal](parseValue: Parser[A]):

  def parse(string: String): Checked[RangeSet[A]] =
    if string.forall(isWhiteChar) then
      Right(RangeSet.empty)
    else
      checkedParse(string, rangeSet)

  private val delimiter: Parser0[Unit] =
    string(",") *> w

  private val range: Parser[RangeSet.Range[A]] =
    ((parseValue <* w) ~ (string(rangeSymbol) *> w *> parseValue <* w).?)
      .map:
        case (a, None) => Single(a)
        case (a, Some(b)) => Interval(a, b)

  private val ranges: Parser[List[RangeSet.Range[A]]] =
    (range ~ (delimiter.with1 *> range).rep0)
      .map { case (head, tail) => head :: tail }

  private val rangeSet: Parser[RangeSet[A]] =
    ((w.with1 *> ranges) <* end)
      .map(RangeSet.fromRanges(_))


object RangeSetParser:
  val rangeSymbol = ".."
  val delimiter = ","
