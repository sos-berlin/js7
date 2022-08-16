package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.test.Test
import js7.data.item.ItemRevision
import js7.data.workflow.position.Label
import js7.tester.CirceJsonTester.testJson

final class WorkflowPathControlTest extends Test
{
  "JSON" in {
    testJson(
      WorkflowPathControl(
        WorkflowPathControlPath(WorkflowPath("WORKFLOW")),
        suspended = true,
        skip = Set(Label("LABEL")),
        Some(ItemRevision(1))),
      json"""{
        "path": "WORKFLOW",
        "suspended": true,
        "skip": [ "LABEL" ],
        "itemRevision": 1
      }""")
  }
}
