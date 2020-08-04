package js7.data.command

import js7.base.circeutils.CirceUtils._
import js7.base.process.ProcessSignal.SIGTERM
import js7.data.item.VersionId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CancelModeTest extends AnyFreeSpec
{
  "JSON" - {
    "NotStarted" in {
      testJson[CancelMode](CancelMode.NotStarted,
        json"""{
          "TYPE": "NotStarted"
         }""")
    }

    "FreshOrStarted" in {
      testJson[CancelMode](CancelMode.FreshOrStarted(),
        json"""{
          "TYPE": "FreshOrStarted"
        } """)
    }

    "FreshOrStarted(Kill)" in {
      testJson[CancelMode](
        CancelMode.FreshOrStarted(Some(
          CancelMode.Kill(
            SIGTERM,
            Some(WorkflowPath("/WORKFLOW") ~ VersionId("VERSION") /: Position(7))))),
        json"""{
          "TYPE": "FreshOrStarted",
            "kill": {
              "signal": "SIGTERM",
              "workflowPosition": {
                "workflowId": {
                  "path": "/WORKFLOW",
                  "versionId": "VERSION"
                },
                "position": [ 7 ]
              }
            }
          }""")
    }
  }
}
