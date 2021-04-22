package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.lock.LockPath
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class LockInstructionTest extends AnyFreeSpec {

  "JSON" in {
    testJson[Instruction.Labeled](
      LockInstruction(LockPath("LOCK"), count = None, lockedWorkflow = Workflow.of()),
      json"""{
        "TYPE": "Lock",
        "lockPath": "LOCK",
        "lockedWorkflow": {
          "instructions": []
        }
      }""")

    testJson[Instruction.Labeled](
      LockInstruction(LockPath("LOCK"), count = Some(3), lockedWorkflow = Workflow.of(), sourcePos = Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Lock",
        "lockPath": "LOCK",
        "count": 3,
        "lockedWorkflow": {
          "instructions": []
        },
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
