package js7.data.subagent

import js7.base.test.OurTestSuite

final class SubagentBundleIdTest extends OurTestSuite:

  "SubagentBundleId" in:
    assert(SubagentBundleId("BUNDLE").toString == "SubagentBundle:BUNDLE")
    assert(SubagentBundleId.pathTypeName == "SubagentBundle")
    assert(SubagentBundleId.pathTypeNames == Seq("SubagentBundle", "SubagentSelection"))
    assert(SubagentBundleId.itemTypeName == "SubagentBundle")
