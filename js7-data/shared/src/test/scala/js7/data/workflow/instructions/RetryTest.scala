package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.tester.CirceJsonTester.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RetryTest extends AnyFreeSpec
{
  "JSON" in {
    testJson[Instruction.Labeled](Retry(Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Retry",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
