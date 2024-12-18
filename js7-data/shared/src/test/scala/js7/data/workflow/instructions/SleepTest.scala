package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SleepTest extends AnyFreeSpec:

  "JSON" in:
    // 3s
    testJson[Instruction](Sleep(expr("3")),
      json"""{
        "TYPE": "Sleep",
        "duration": "3"
      }""")

    // 3ms
    testJson[Instruction](Sleep(expr("0.003")),
      json"""{
        "TYPE": "Sleep",
        "duration": "0.003"
      }""")
