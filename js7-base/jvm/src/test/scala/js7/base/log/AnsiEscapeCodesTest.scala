package js7.base.log

import js7.base.log.AnsiEscapeCodes.{blue, bold, highlight, removeHighlights}
import js7.base.test.OurTestSuite

final class AnsiEscapeCodesTest extends OurTestSuite:

  "removeHighlights" - {
    "removeHighlights" in:
      val line = highlight(blue)(s"2026-02-12T12:00:00.000000 [info] thread class - ${bold("MESSAGE")}")
      assert(removeHighlights(line) == "2026-02-12T12:00:00.000000 [info] thread class - MESSAGE")

    "does not copy String when nothing as been removed" in:
      val string = "x"
      assert(removeHighlights(string) eq string)
  }