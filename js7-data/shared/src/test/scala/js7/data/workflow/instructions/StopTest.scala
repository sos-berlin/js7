package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class StopTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction.Labeled](
      Stop(sourcePos = Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Stop",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
