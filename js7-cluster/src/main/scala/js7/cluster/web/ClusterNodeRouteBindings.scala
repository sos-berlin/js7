package js7.cluster.web

import js7.cluster.ClusterNode
import js7.data.cluster.{ClusterCommand, ClusterWatchingCommand}
import js7.data.event.SnapshotableState

trait ClusterNodeRouteBindings[S <: SnapshotableState[S]] {

  protected def clusterNode: ClusterNode[S]

  protected final def executeClusterCommand(command: ClusterCommand) =
    clusterNode.executeCommand(command)

  protected final def executeClusterWatchingCommand(cmd: ClusterWatchingCommand) =
    clusterNode.executeClusterWatchingCommand(cmd)

  protected final val clusterWatchRequestStream =
    clusterNode.clusterWatchRequestStream
}
