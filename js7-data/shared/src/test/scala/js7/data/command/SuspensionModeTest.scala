package js7.data.command

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.data.item.VersionId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

/**
  * @author Joacim Zschimmer
  */
final class SuspensionModeTest extends OurTestSuite:

  "JSON" - {
    "SuspensionMode(resetState, Kill())" in:
      testJson[SuspensionMode](
        SuspensionMode(resetState = true, Some(CancellationMode.Kill())),
        json"""{
          "resetState": true,
          "kill": {
            "immediately": false
          }
        }""")

      testJsonDecoder(
        SuspensionMode(resetState = false, Some(CancellationMode.Kill())),
        json"""{
          "kill": {}
        }""")

    "SuspensionMode(Kill(...))" in:
      testJson[SuspensionMode](
        SuspensionMode(
          resetState = true,
          kill = Some(
            CancellationMode.Kill(
              immediately = true,
              Some(WorkflowPath("WORKFLOW") ~ VersionId("VERSION") /: Position(7))))),
        json"""{
          "resetState": true,
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
