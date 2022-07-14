package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GapTest extends AnyFreeSpec {

  // For internal JS7 use only.

  "JSON" in {
    testJson[Instruction.Labeled](
      Gap(Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Gap",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
