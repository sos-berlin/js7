package js7.data.command

import js7.base.circeutils.CirceUtils.*
import js7.data.item.VersionId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SuspensionModeTest extends AnyFreeSpec
{
  "JSON" - {
    "SuspensionMode(Kill())" in {
      testJson[SuspensionMode](
        SuspensionMode(Some(CancellationMode.Kill())),
        json"""{
          "kill": {
            "immediately": false
          }
        }""")

      assert(json"""{
        "kill": {}
      }""".as[SuspensionMode] == Right(SuspensionMode(Some(CancellationMode.Kill()))))
    }

    "SuspensionMode(Kill(...))" in {
      testJson[SuspensionMode](
        SuspensionMode(Some(
          CancellationMode.Kill(
            immediately = true,
            Some(WorkflowPath("WORKFLOW") ~ VersionId("VERSION") /: Position(7))))),
        json"""{
          "kill": {
            "immediately": true,
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
                "versionId": "VERSION"
              },
              "position": [ 7 ]
            }
          }
        }""")
    }
  }
}
