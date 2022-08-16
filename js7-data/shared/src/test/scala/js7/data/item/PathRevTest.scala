package js7.data.item

import js7.base.test.Test
import js7.data.subagent.SubagentId

final class PathRevTest extends Test
{
  "toString" in {
    assert(PathRev(SubagentId("SUBAGENT"), Some(ItemRevision(123))).toString ==
      "Subagent:SUBAGENT~123")
    assert(PathRev(SubagentId("SUBAGENT"), None).toString ==
      "Subagent:SUBAGENT")
  }
}
