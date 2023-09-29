package js7.data.item

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowControl, WorkflowControlPath}
import js7.tester.CirceJsonTester.testJson

final class UnsignedItemEventTest extends OurTestSuite:
  import js7.data.controller.ControllerState.implicitItemContainer

  "UnsignedItemAdded" in:
    testJson[UnsignedItemEvent](
      UnsignedItemAdded(WorkflowControl(
        WorkflowControlPath("WORKFLOW") ~ "1",
        Set(Position(1)),
        Some(ItemRevision(1)))),
      json"""{
        "TYPE": "UnsignedItemAdded",
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

  "UnsignedItemChanged" in:
    testJson[UnsignedItemEvent](
      UnsignedItemChanged(WorkflowControl(
        WorkflowControlPath("WORKFLOW") ~ "1",
        Set(Position(1)),
        Some(ItemRevision(1)))),
      json"""{
        "TYPE": "UnsignedItemChanged",
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
