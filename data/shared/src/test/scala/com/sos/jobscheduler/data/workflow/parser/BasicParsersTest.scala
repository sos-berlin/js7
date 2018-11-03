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
      assert(quotedString.checkedParse("""''""") == Valid(""))
      assert(quotedString.checkedParse("""'x'""") == Valid("x"))
      assert(quotedString.checkedParse("""'ö'""") == Valid("ö"))
      assert(quotedString.checkedParse("""'x\n'""") == Valid("""x\n"""))
      assert(quotedString.checkedParse("""'x$y'""") == Valid("x$y"))
    }
  }

  "String in double-quotes" - {
    "Backslash is not supported" in {
      // TODO Backslash in string
      assert(quotedString.checkedParse(""""x\n"""") == Invalid(Problem(
        """Double-quoted (") string is not properly terminated or contains a non-printable character or backslash (\):1:2 ..."x\\n\""""")))
    }

    "String interpolation is not supported" in {
      // TODO String interpolation
      assert(quotedString.checkedParse(""""x$variable"""") == Invalid(Problem(
        """Variable interpolation via '$' in double-quoted is not implemented. Consider using single quotes ('):1:13 ...""""")))
    }

    "Valid" in {
      assert(quotedString.checkedParse("""""""") == Valid(""))
      assert(quotedString.checkedParse(""""x"""") == Valid("x"))
      assert(quotedString.checkedParse(""""ö"""") == Valid("ö"))
    }
  }

  "keyValues" - {
    val parser = keyValueMap(Map("string" → quotedString, "number" → int))

    "Duplicate keys" in {
      assert(parser.checkedParse("""string="A", string="B"""") == Invalid(Problem("""Duplicate keywords: string:1:23 ...""""")))
    }

    "Valid" in {
      val parser = keyValueMap(Map("string" → quotedString, "number" → int))
      assert(parser.checkedParse("""string="STRING", number=7""") == Valid(KeyToValue(Map("string" → "STRING", "number" → 7))))
    }
  }

  "KeyToValue" - {
    val keyToValue = KeyToValue(Map("string" → "STRING", "string2" → "STRING2", "number" → 7))

    // checkedParse("") in following tests due to Parser.flatMap

    "apply failed" in {
      assert(keyToValue[String]("MISSING").checkedParse("") == Invalid(Problem("""Missing required argument 'MISSING=':1:1 ...""""")))
    }

    "apply" in {
      assert(keyToValue[String]("string").checkedParse("") == Valid("STRING"))
    }

    "apply with default" in {
      assert(keyToValue[String]("string", "DEFAULT").checkedParse("") == Valid("STRING"))
      assert(keyToValue[String]("MISSING", "DEFAULT").checkedParse("") == Valid("DEFAULT"))
    }

    "oneOf" in {
      assert(keyToValue.oneOf[String](Set("string", "string2")).checkedParse("") == Invalid(Problem("""Contradicting keywords: string; string2:1:1 ...""""")))
      assert(keyToValue.oneOf[String](Set("string", "x")).checkedParse("") == Valid("string" → "STRING"))
      assert(keyToValue.oneOf[String](Set("string2", "x")).checkedParse("") == Valid("string2" → "STRING2"))
      assert(keyToValue.oneOf[String](Set("y", "x")).checkedParse("") == Invalid(Problem("""Missing one of the keywords: y, x:1:1 ...""""")))
    }

    "oneOfOr" in {
      assert(keyToValue.oneOfOr[String](Set("string", "string2"), "DEFAULT").checkedParse("") == Invalid(Problem("""Contradicting keywords: string; string2:1:1 ...""""")))
      assert(keyToValue.oneOfOr[String](Set("string", "x"), "DEFAULT").checkedParse("") == Valid("STRING"))
      assert(keyToValue.oneOfOr[String](Set("string2", "x"), "DEFAULT").checkedParse("") == Valid("STRING2"))
      assert(keyToValue.oneOfOr[String](Set("y", "x"), "DEFAULT").checkedParse("") == Valid("DEFAULT"))
    }

    "noneOrOneOf" in {
      assert(keyToValue.noneOrOneOf[String](Set("string", "string2")).checkedParse("") == Invalid(Problem("""Contradicting keywords: string; string2:1:1 ...""""")))
      assert(keyToValue.noneOrOneOf[String](Set("string", "x")).checkedParse("") == Valid(Some("string" → "STRING")))
      assert(keyToValue.noneOrOneOf[String](Set("string2", "x")).checkedParse("") == Valid(Some("string2" → "STRING2")))
      assert(keyToValue.noneOrOneOf[String](Set("y", "x")).checkedParse("") == Valid(None))
    }
  }
}
