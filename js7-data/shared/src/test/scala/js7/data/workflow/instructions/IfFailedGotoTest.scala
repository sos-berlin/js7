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
final class IfFailedGotoTest extends AnyFreeSpec {

  // For internal JS7 use only.

  "JSON" in {
    testJson[Instruction.Labeled](
      IfFailedGoto(Label("A"), Some(SourcePos(1, 2))),
      json"""{
        "TYPE": "IfFailedGoto",
        "to": "A",
        "sourcePos": [ 1, 2 ]
      }""")
  }
}
