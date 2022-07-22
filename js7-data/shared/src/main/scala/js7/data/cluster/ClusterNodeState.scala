package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.node.NodeId

final case class ClusterNodeState(
  nodeId: NodeId,
  isBackup: Boolean,
  clusterState: ClusterState)
{
  def isActive: Boolean =
    clusterState match {
      case ClusterState.Empty => !isBackup
      case o: ClusterState.HasNodes => nodeId == o.activeId
    }
}

object ClusterNodeState
{
  implicit val jsonCodec: Codec.AsObject[ClusterNodeState] = deriveCodec
}
