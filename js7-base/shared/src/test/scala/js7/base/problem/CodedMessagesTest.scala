package js7.base.problem

import js7.base.problem.CodedMessages.patternToMessage
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class CodedMessagesTest extends OurTestSuite
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
