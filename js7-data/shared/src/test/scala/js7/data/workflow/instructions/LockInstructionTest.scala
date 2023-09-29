package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.data.lock.LockPath
import js7.data.order.OrderEvent.LockDemand
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Workflow}
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

final class LockInstructionTest extends OurTestSuite:

  "JSON" in:
    // COMPATIBLE with v2.4
    testJsonDecoder[Instruction.Labeled](
      LockInstruction.single(LockPath("LOCK"), count = None, lockedWorkflow = Workflow.of()),
      json"""{
        "TYPE": "Lock",
        "lockPath": "LOCK",
        "lockedWorkflow": {
          "instructions": []
        }
      }""")

    // COMPATIBLE with v2.4
    testJsonDecoder[Instruction.Labeled](
      LockInstruction.single(
        LockPath("LOCK"),
        count = Some(3),
        lockedWorkflow = Workflow.of(),
        sourcePos = Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Lock",
        "lockPath": "LOCK",
        "count": 3,
        "lockedWorkflow": {
          "instructions": []
        },
        "sourcePos": [ 1, 2 ]
      }""")

    testJson[Instruction.Labeled](
      LockInstruction(
        List(
          LockDemand(LockPath("LOCK"))),
        lockedWorkflow = Workflow.of()),
      json"""{
        "TYPE": "Lock",
        "demands": [
          {
            "lockPath": "LOCK"
          }
        ],
        "lockedWorkflow": {
          "instructions": []
        }
      }""")

    testJson[Instruction.Labeled](
      LockInstruction(
        List(
          LockDemand(LockPath("LOCK"), count = Some(3))),
        lockedWorkflow = Workflow.of(),
        sourcePos = Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Lock",
        "demands": [
          {
            "lockPath": "LOCK",
            "count": 3
          }
        ],
        "lockedWorkflow": {
          "instructions": []
        },
        "sourcePos": [ 1, 2 ]
      }""")

    assert(
      json"""{
        "TYPE": "Lock",
        "demands": [
          { "lockPath": "LOCK" },
          { "lockPath": "LOCK" }
        ],
        "lockedWorkflow": {
          "instructions": []
        }
      }""".checkedAs[LockInstruction] == Left(Problem(
        "JSON DecodingFailure at : Unexpected duplicates: 2×Lock:LOCK")))

    assert(
      json"""{
        "TYPE": "Lock",
        "demands": [
          { "lockPath": "LOCK", "count": 0 }
        ],
        "lockedWorkflow": {
          "instructions": []
        }
      }""".checkedAs[LockInstruction] == Left(Problem(
        "JSON DecodingFailure at : LockDemand.count must not be below 1 for Lock:LOCK")))
