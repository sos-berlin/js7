package js7.data.cluster

import io.circe.Codec
import io.circe.generic.semiauto

final case class ClusterStateSnapshot(clusterState: ClusterState)

object ClusterStateSnapshot {
  implicit val jsonCodec: Codec.AsObject[ClusterStateSnapshot] = semiauto.deriveCodec
}
