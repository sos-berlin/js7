package js7.data.cluster

import js7.base.web.Uri
import js7.data.cluster.ClusterSetting._
import js7.data.cluster.ClusterSetting.syntax._
import js7.data.node.NodeId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterSettingTest extends AnyFreeSpec
{
  private val idToUri = Map(
    NodeId("A") -> Uri("http://A"),
    NodeId("B") -> Uri("http://B"))

  "checked" in {
    assert(checkUris(Map.empty).isLeft)
    assert(checkUris(Map(NodeId("A") -> Uri("http://A"))).isLeft)
    assert(checkUris(Map(NodeId("A") -> Uri("http://SAME"), NodeId("B") -> Uri("http://SAME"))).isLeft)
    assert(ClusterSetting.checked(idToUri, NodeId("X")).isLeft)

    assert(ClusterSetting.checked(idToUri, NodeId("A")).isRight)
    assert(checkUris(idToUri).isRight)
  }

  "peerOf" in {
    assert(idToUri.peerOf(NodeId("A")) == NodeId("B"))
    assert(idToUri.peerOf(NodeId("B")) == NodeId("A"))
  }
}
