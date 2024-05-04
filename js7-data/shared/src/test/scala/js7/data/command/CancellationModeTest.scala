package js7.data.command

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.item.VersionId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class CancellationModeTest extends OurTestSuite:

  "JSON" - {
    "FreshOnly" in:
      testJson[CancellationMode](CancellationMode.FreshOnly,
        json"""{
          "TYPE": "FreshOnly"
         }""")

    "FreshOrStarted" in:
      testJson[CancellationMode](CancellationMode.FreshOrStarted(),
        json"""{
          "TYPE": "FreshOrStarted"
        } """)

    "FreshOrStarted(Kill)" in:
      testJson[CancellationMode](
        CancellationMode.FreshOrStarted(Some(CancellationMode.Kill())),
        json"""{
          "TYPE": "FreshOrStarted",
          "kill": {
            "immediately": false
          }
        }""")

      assert(json"""{
        "TYPE": "FreshOrStarted",
        "kill": {}
      }""".as[CancellationMode] == Right(CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))))

    "FreshOrStarted(Kill(...))" in:
      testJson[CancellationMode](
        CancellationMode.FreshOrStarted(Some(
          CancellationMode.Kill(
            immediately = true,
            Some(WorkflowPath("WORKFLOW") ~ VersionId("VERSION") /: Position(7))))),
        json"""{
            "TYPE": "FreshOrStarted",
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
