package js7.data.item

import js7.data.subagent.SubagentId
import org.scalatest.freespec.AnyFreeSpec

final class PathRevTest extends AnyFreeSpec
{
  "toString" in {
    assert(PathRev(SubagentId("SUBAGENT"), Some(ItemRevision(123))).toString ==
      "Subagent:SUBAGENT~123")
    assert(PathRev(SubagentId("SUBAGENT"), None).toString ==
      "Subagent:SUBAGENT")
  }
}
