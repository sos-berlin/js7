package js7.base.utils

import js7.base.parser.BasicParsers
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.utils.RangeSet.{Interval, Single}

final class RangeSetParserTest extends OurTestSuite:

  private def parse(string: String) =
    RangeSet.parse(BasicParsers.int, string)

  "Empty" in:
    assert(parse("") == Right(RangeSet.empty[Int]))

  "Invalid cases" - {
    "Big number" in:
      val tooBig = 2147483648L
      assert(tooBig == Int.MaxValue.toLong + 1)
      assert(parse(s"$tooBig") == Left(Problem(
        """Parsing failed at position 11 “2147483648❓” · NumberFormatException: For input string: "2147483648"""".stripMargin)))

    "No delimiter" in:
      // Should not be parsed like "1,-3"
      assert(parse("1-3") == Left(Problem(
        "Parsing failed at position 2 “1❓-3” · Expected end of input")))
  }

  "Single value" in:
    assert(parse("  ") == Right(RangeSet.empty[Int]))
    assert(parse("1") == Right(RangeSet.fromRanges(Vector(Single(1)))))
    assert(parse("  1  ") == Right(RangeSet.fromRanges(Vector(Single(1)))))

  "Ranges" in:
    val rangeSet = RangeSet.fromRanges(Vector(Interval(1, 3), Single(5), Interval(7, 9)))
    assert(parse("1..3,5,7..9") == Right(rangeSet))
    assert(parse(" 1..3,5  ,  7  ..  9 ") == Right(rangeSet))
