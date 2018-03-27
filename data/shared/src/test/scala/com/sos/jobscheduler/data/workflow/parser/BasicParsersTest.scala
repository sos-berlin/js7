package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import com.sos.jobscheduler.data.workflow.parser.Parsers.ops._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class BasicParsersTest extends FreeSpec {

  "String in single-quotes" - {
    "Single-quote itself cannot be used" in {
      assert(quotedString.checkedParse("""'\''""") == Invalid(Problem("""End:1:4 ..."'"""")))
    }

    "Control character is not allowed" in {
      assert(quotedString.checkedParse("'\t'") == Invalid(Problem(
        """Single-quoted (') string is not properly terminated or contains a non-printable character:1:2 ..."\t'"""")))
      assert(quotedString.checkedParse("'\n'") == Invalid(Problem(
        """Single-quoted (') string is not properly terminated or contains a non-printable character:1:2 ..."\n'"""")))
    }

    "Valid" in {
      assert(quotedString.checkedParse("""'x'""") == Valid("x"))
      assert(quotedString.checkedParse("""'x\n'""") == Valid("""x\n"""))
      assert(quotedString.checkedParse("""'x$y'""") == Valid("x$y"))
    }
  }

  "String in double-quotes" - {
    "Backslash is not supported" in {
      // TODO Backslash in string
      assert(quotedString.checkedParse(""""x\n"""") == Invalid(Problem(
        """Double-quoted (") string is not properly terminated or contains a non-printable character or backslash (\):1:1 ..."\"x\\n\""""")))
    }

    "String interpolation is not supported" in {
      // TODO String interpolation
      assert(quotedString.checkedParse(""""x$variable"""") == Invalid(Problem(
        """Variable interpolation via '$' in double-quoted is not implemented. Consider using single quotes ('):1:13 ...""""")))
    }

    "Valid" in {
      assert(quotedString.checkedParse(""""x"""") == Valid("x"))
    }
  }
}
