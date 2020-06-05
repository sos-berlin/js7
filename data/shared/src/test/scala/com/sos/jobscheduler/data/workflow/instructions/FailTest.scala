package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.expression.Expression.StringConstant
import js7.data.job.ReturnCode
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FailTest extends AnyFreeSpec
{
  "JSON" - {
    "with defaults" in {
      testJson[Instruction.Labeled](
        Fail(None),
        json"""{
          "TYPE": "Fail"
        }""")
    }

    "complete" in {
      testJson[Instruction.Labeled](
        Fail(Some(StringConstant("ERROR")), Some(ReturnCode(7)), uncatchable = true, Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Fail",
          "message": "'ERROR'",
          "returnCode": 7,
          "uncatchable": true,
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}
