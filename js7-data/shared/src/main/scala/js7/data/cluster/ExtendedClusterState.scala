package js7.data.cluster

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.data.node.NodeId

final case class ExtendedClusterState(
  nodeId: NodeId,
  clusterState: ClusterState)

object ExtendedClusterState
{
  implicit val jsonCodec = deriveCodec[ExtendedClusterState]
}
