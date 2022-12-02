package js7.data.cluster

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.ProblemException
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting.*
import js7.data.cluster.ClusterSetting.syntax.*
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class ClusterSettingTest extends OurTestSuite
{
  private val idToUri = Map(
    NodeId("A") -> Uri("https://A"),
    NodeId("B") -> Uri("https://B"))
  private val timing = ClusterTiming(1.s, 2.s)
  private val clusterSetting = ClusterSetting(
    idToUri,
    NodeId("A"),
    timing)

  "JSON" in {
    testJson(clusterSetting,
      json"""{
        "idToUri": {
          "A": "https://A",
          "B": "https://B"
        },
        "activeId": "A",
        "timing": {
          "heartbeat": 1,
          "heartbeatTimeout": 2
        }
      }""")
  }

  "checked" in {
    assert(checkUris(Map.empty).isLeft)
    assert(checkUris(Map(NodeId("A") -> Uri("https://A"))).isLeft)
    assert(checkUris(Map(NodeId("A") -> Uri("https://SAME"), NodeId("B") -> Uri("https://SAME"))).isLeft)

    assert(ClusterSetting.checked(idToUri, NodeId("X"), timing).isLeft)
    assert(ClusterSetting.checked(idToUri, NodeId("A"), timing).isRight)
    assert(checkUris(idToUri).isRight)
  }

  "apply" in {
    intercept[ProblemException](ClusterSetting(idToUri, NodeId("X"), timing))
  }

  "activeId" in {
    assert(clusterSetting.activeId == NodeId("A"))
  }

  "activeUri" in {
    assert(clusterSetting.activeUri == Uri("https://A"))
  }

  "passiveId" in {
    assert(clusterSetting.passiveId == NodeId("B"))
  }

  "passiveUri" in {
    assert(clusterSetting.passiveUri == Uri("https://B"))
  }

  "peerOf" in {
    assert(idToUri.peerOf(NodeId("A")) == NodeId("B"))
    assert(idToUri.peerOf(NodeId("B")) == NodeId("A"))
    intercept[AssertionError](idToUri.peerOf(NodeId("X")))
  }

  "withPassiveUri" in {
    assert(clusterSetting.withPassiveUri(Uri("https://UPDATED")) ==
      clusterSetting.copy(idToUri = Map(
        NodeId("A") -> Uri("https://A"),
        NodeId("B") -> Uri("https://UPDATED"))))
  }
}
