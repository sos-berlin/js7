package js7.core.cluster

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.JsonObject
import js7.base.generic.Completed
import js7.base.problem.{Checked, Problem}
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers.*
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerId
import monix.eval.Task
import monix.execution.Scheduler

trait ClusterWatchRoute
{
  protected def clusterWatchRegister: ClusterWatchRegister
  protected def scheduler: Scheduler

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final def clusterWatchRoute(controllerId: ControllerId): Route =
    pathEnd {
      post {
        entity(as[ClusterWatchMessage]) {
          case clusterWatchEvents: ClusterWatchEvents =>
            complete(
              clusterWatchRegister(controllerId)
                .flatMap(_.applyEvents(clusterWatchEvents))
                .map(_.map((_: Completed) => JsonObject.empty))
                .runToFuture)

          case ClusterWatchHeartbeat(from, clusterState) =>
            complete(
              clusterWatchRegister(controllerId)
                .flatMap(_.heartbeat(from, clusterState))
                .map(_.map((_: Completed) => JsonObject.empty))
                .runToFuture)
        }
      } ~
      get {
        complete(
          clusterWatchRegister.tryRead(controllerId)
            .flatMap[Checked[ClusterState]] {
              case None =>
                Task.pure(Problem(s"No ClusterState registered for '$controllerId'"))
              case Some(clusterWatch) =>
                clusterWatch.get
            }.runToFuture)
      }
    }
}
