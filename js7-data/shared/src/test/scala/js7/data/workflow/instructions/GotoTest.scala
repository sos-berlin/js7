package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.{Instruction, Label}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GotoTest extends AnyFreeSpec {

  // For compatibility with JS7 1 only.

  "JSON" in {
    testJson[Instruction.Labeled](
      Goto(Label("A"), Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "Goto",
        "to": "A",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
