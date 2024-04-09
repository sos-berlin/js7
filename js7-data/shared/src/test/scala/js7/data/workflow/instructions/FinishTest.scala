package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.order.OrderOutcome
import js7.data.source.SourcePos
import js7.data.value.NumberValue
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

/**
  * @author Joacim Zschimmer
  */
final class FinishTest extends OurTestSuite
{
  "JSON" - {
    "with defaults" in {
      testJson[Instruction.Labeled](
        Finish(None),
        json"""{
          "TYPE": "Finish"
        }""")

      testJsonDecoder[Instruction.Labeled](
        Finish(Some(OrderOutcome.failed)),
        json"""{
          "TYPE": "Finish",
          "outcome": {
            "TYPE": "Failed"
          }
        }""")

      testJsonDecoder[Instruction.Labeled](
        Finish(Some(OrderOutcome.succeeded)),
        json"""{
          "TYPE": "Finish",
          "outcome": {
            "TYPE": "Succeeded",
            "namedValues": {}
          }
        }""")
    }

    "complete" in {
      testJson[Instruction.Labeled](
        Finish(
          Some(
            OrderOutcome.Failed(
              Some("MESSAGE"),
              Map("A" -> NumberValue(7)))),
          Some(SourcePos(1, 2))),
        json"""{
          "TYPE": "Finish",
          "outcome": {
            "TYPE": "Failed",
            "message": "MESSAGE",
            "namedValues": {
              "A": 7
            }
          },
          "sourcePos": [ 1, 2 ]
        }""")
    }
  }
}
