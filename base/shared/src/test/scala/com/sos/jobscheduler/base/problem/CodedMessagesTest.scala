package com.sos.jobscheduler.base.problem

import com.sos.jobscheduler.base.problem.CodedMessages.patternToMessage
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CodedMessagesTest extends FreeSpec
{
  "patternToMessage" - {
    "without arguments" in {
      assert(patternToMessage("PATTERN", Map.empty) == "PATTERN")
    }

    "with arguments" in {
      assert(patternToMessage("PATTERN first=$first", Map("first" -> "FIRST")) == "PATTERN first=FIRST")
    }

    "with too much arguments" in {
      assert(
        patternToMessage(
          "PATTERN first=$first third=$third",
          Map("first" -> "FIRST", "second" -> "SECOND", "third" -> "THIRD", "forth" -> "FORTH")
        ) == "PATTERN first=FIRST third=THIRD (second=SECOND, forth=FORTH)")
    }

    "with missing arguments" in {
      assert(patternToMessage("PATTERN first=$first third=$third", Map("first" -> "FIRST")) == "PATTERN first=FIRST third=$third")
    }
  }
}
