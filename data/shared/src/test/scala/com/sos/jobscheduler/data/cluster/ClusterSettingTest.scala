package com.sos.jobscheduler.data.cluster

import com.sos.jobscheduler.data.cluster.ClusterSetting.syntax._
import com.sos.jobscheduler.data.cluster.ClusterSetting._
import com.sos.jobscheduler.data.common.Uri
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterSettingTest extends FreeSpec
{
  private val idToUri = Map(
    ClusterNodeId("A") -> Uri("http://A"),
    ClusterNodeId("B") -> Uri("http://B"))

  "checkUris" in {
    assert(checkUris(Map.empty).isLeft)
    assert(checkUris(Map(ClusterNodeId("A") -> Uri("http://A"))).isLeft)
    assert(checkUris(Map(ClusterNodeId("A") -> Uri("http://SAME"), ClusterNodeId("B") -> Uri("http://SAME"))).isLeft)
    assert(checkUris(idToUri, ClusterNodeId("X")).isLeft)

    assert(checkUris(idToUri, ClusterNodeId("A")).isRight)
    assert(checkUris(idToUri).isRight)
  }

  "peerOf" in {
    assert(idToUri.peerOf(ClusterNodeId("A")) == ClusterNodeId("B"))
    assert(idToUri.peerOf(ClusterNodeId("B")) == ClusterNodeId("A"))
  }
}
