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
final class CancelModeTest extends AnyFreeSpec
{
  "JSON" - {
    "FreshOnly" in {
      testJson[CancelMode](CancelMode.FreshOnly,
        json"""{
          "TYPE": "FreshOnly"
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
        CancelMode.FreshOrStarted(Some(CancelMode.Kill())),
        json"""{
          "TYPE": "FreshOrStarted",
          "kill": {
            "immediately": false
          }
        }""")

      assert(json"""{
        "TYPE": "FreshOrStarted",
        "kill": {}
      }""".as[CancelMode] == Right(CancelMode.FreshOrStarted(Some(CancelMode.Kill()))))
    }

    "FreshOrStarted(Kill(...))" in {
      testJson[CancelMode](
        CancelMode.FreshOrStarted(Some(
          CancelMode.Kill(
            immediately = true,
            Some(WorkflowPath("/WORKFLOW") ~ VersionId("VERSION") /: Position(7))))),
        json"""{
            "TYPE": "FreshOrStarted",
            "kill": {
              "immediately": true,
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
