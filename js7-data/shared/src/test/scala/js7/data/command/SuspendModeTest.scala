package js7.data.command

import js7.base.circeutils.CirceUtils._
import js7.data.item.VersionId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SuspendModeTest extends AnyFreeSpec
{
  "JSON" - {
    "SuspendMode(Kill())" in {
      testJson[SuspendMode](
        SuspendMode(Some(CancelMode.Kill())),
        json"""{
          "kill": {
            "immediately": false
          }
        }""")

      assert(json"""{
        "kill": {}
      }""".as[SuspendMode] == Right(SuspendMode(Some(CancelMode.Kill()))))
    }

    "SuspendMode(Kill(...))" in {
      testJson[SuspendMode](
        SuspendMode(Some(
          CancelMode.Kill(
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
