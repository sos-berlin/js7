package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.item.ItemRevision
import js7.data.workflow.position.Label
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowPathControlTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      WorkflowPathControl(
        WorkflowPath("WORKFLOW"),
        suspended = true,
       skip = Set(Label("LABEL")),
        ItemRevision(1)),
      json"""{
        "path": "WORKFLOW",
        "suspended": true,
        "skip": [ "LABEL" ],
        "revision": 1
      }""")
  }
}
