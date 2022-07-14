package js7.controller.web.controller.api

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Directives.*
import js7.base.auth.ValidUserPermission
import js7.base.problem.Checked
import js7.common.akkahttp.CirceJsonSupport.*
import js7.common.akkahttp.StandardMarshallers.*
import js7.controller.web.common.ControllerRouteProvider
import js7.data.cluster.{ClusterNodeState, ClusterState}
import js7.data.node.NodeId
import monix.eval.Task

trait ClusterRoute extends ControllerRouteProvider
{
  private implicit def implicitScheduler = scheduler

  protected def checkedClusterState: Task[Checked[ClusterState]]
  protected def clusterNodeIsBackup: Boolean
  protected def nodeId: NodeId

  protected final lazy val clusterRoute =
    authorizedUser(ValidUserPermission) { _ =>
      get {
        pathEnd {
          parameter("return".?) { maybeReturn =>
            complete(
              checkedClusterState
                .map(_.map[ToResponseMarshallable](clusterState =>
                  if (maybeReturn contains "ClusterNodeState")
                    ClusterNodeState(nodeId, isBackup = clusterNodeIsBackup, clusterState)
                  else
                    clusterState))
                .runToFuture)
          }
        }
      }
    }
}
