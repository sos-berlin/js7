package js7.base.log

import js7.base.test.OurTestSuite
import AnsiEscapeCodes.{blue, bold, highlight, removeHighlights}

final class AnsiEscapeCodesTest extends OurTestSuite:

  "removeHighlights" in :
    val line = highlight(blue)(s"2026-02-12T12:00:00.000000 [info] thread class - ${bold("MESSAGE")}")
    assert(removeHighlights(line) == "2026-02-12T12:00:00.000000 [info] thread class - MESSAGE")


