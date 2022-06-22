package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.item.ItemRevision
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowPathControlTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(
      WorkflowPathControl(
        WorkflowPath("WORKFLOW"),
        suspended = true,
        ItemRevision(1)),
      json"""{
        "path": "WORKFLOW",
        "suspended": true,
        "revision": 1
      }""")
  }
}
