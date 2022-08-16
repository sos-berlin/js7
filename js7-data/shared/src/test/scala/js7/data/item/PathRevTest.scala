package js7.data.item

import js7.base.test.OurTestSuite
import js7.data.subagent.SubagentId

final class PathRevTest extends OurTestSuite
{
  "toString" in {
    assert(PathRev(SubagentId("SUBAGENT"), Some(ItemRevision(123))).toString ==
      "Subagent:SUBAGENT~123")
    assert(PathRev(SubagentId("SUBAGENT"), None).toString ==
      "Subagent:SUBAGENT")
  }
}
