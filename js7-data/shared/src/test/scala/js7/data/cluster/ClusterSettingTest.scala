package js7.data.cluster

import js7.base.circeutils.CirceUtils._
import js7.base.problem.ProblemException
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.data.cluster.ClusterSetting._
import js7.data.cluster.ClusterSetting.syntax._
import js7.data.node.NodeId
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterSettingTest extends AnyFreeSpec
{
  private val idToUri = Map(
    NodeId("A") -> Uri("http://A"),
    NodeId("B") -> Uri("http://B"))
  private val timing = ClusterTiming(1.s, 2.s)
  private val clusterSetting = ClusterSetting(
    idToUri,
    NodeId("A"),
    Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
    timing)

  "JSON" in {
    testJson(clusterSetting,
      json"""{
        "idToUri": {
          "A": "http://A",
          "B": "http://B"
        },
        "activeId": "A",
        "clusterWatches": [
          {
            "uri": "https://CLUSTER-WATCH"
          }
        ],
        "timing": {
          "heartbeat": 1,
          "heartbeatTimeout": 2
        }
      }""")
  }

  "checked" in {
    assert(checkUris(Map.empty).isLeft)
    assert(checkUris(Map(NodeId("A") -> Uri("http://A"))).isLeft)
    assert(checkUris(Map(NodeId("A") -> Uri("http://SAME"), NodeId("B") -> Uri("http://SAME"))).isLeft)

    assert(ClusterSetting.checked(idToUri, NodeId("X"), Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))), timing).isLeft)

    assert(ClusterSetting.checked(idToUri, NodeId("A"), Nil, timing).isLeft)
    assert(ClusterSetting.checked(idToUri, NodeId("A"), Seq(ClusterSetting.Watch(Uri("https://V")), ClusterSetting.Watch(Uri("https://W"))), timing).isLeft)
    assert(ClusterSetting.checked(idToUri, NodeId("A"), Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))), timing).isRight)
    assert(checkUris(idToUri).isRight)
  }

  "apply" in {
    intercept[ProblemException](ClusterSetting(idToUri, NodeId("X"), Nil, timing))
  }

  "activeId" in {
    assert(clusterSetting.activeId == NodeId("A"))
  }

  "activeUri" in {
    assert(clusterSetting.activeUri == Uri("http://A"))
  }

  "passiveId" in {
    assert(clusterSetting.passiveId == NodeId("B"))
  }

  "passiveUri" in {
    assert(clusterSetting.passiveUri == Uri("http://B"))
  }

  "peerOf" in {
    assert(idToUri.peerOf(NodeId("A")) == NodeId("B"))
    assert(idToUri.peerOf(NodeId("B")) == NodeId("A"))
    intercept[AssertionError](idToUri.peerOf(NodeId("X")))
  }

  "withPassiveUri" in {
    assert(clusterSetting.withPassiveUri(Uri("HTTP://UPDATED")) ==
      clusterSetting.copy(idToUri = Map(
        NodeId("A") -> Uri("http://A"),
        NodeId("B") -> Uri("HTTP://UPDATED"))))
  }
}
