package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class SayTest extends AnyFreeSpec:

  "JSON" in:
    testJson[Instruction](Say(expr("'Hello'")),
      json"""{
        "TYPE": "Say",
        "what": "'Hello'"
      }""")
