package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.item.VersionedControlEvent.{VersionedControlAdded, VersionedControlChanged}
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowControl, WorkflowControlPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class VersionedControlEventTest extends AnyFreeSpec
{
  import js7.data.controller.ControllerState.implicitItemContainer

  "VersionedControlAdded" in {
    testJson[VersionedControlEvent](
      VersionedControlAdded(WorkflowControl(
        WorkflowControlPath("WORKFLOW") ~ "1",
        Set(Position(1)),
        Some(ItemRevision(1)))),
      json"""{
        "TYPE": "VersionedControlAdded",
        "item": {
          "TYPE": "WorkflowControl",
          "id": {
            "path": "WORKFLOW",
            "versionId": "1"
          },
          "breakpoints": [ [ 1 ] ],
          "itemRevision": 1
        }
      }""")
  }

  "VersionedControlChanged" in {
    testJson[VersionedControlEvent](
      VersionedControlChanged(WorkflowControl(
        WorkflowControlPath("WORKFLOW") ~ "1",
        Set(Position(1)),
        Some(ItemRevision(1)))),
      json"""{
        "TYPE": "VersionedControlChanged",
        "item": {
          "TYPE": "WorkflowControl",
          "id": {
            "path": "WORKFLOW",
            "versionId": "1"
          },
          "breakpoints": [ [ 1 ] ],
          "itemRevision": 1
        }
      }"""
    )
  }
}
