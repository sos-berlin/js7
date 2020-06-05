package js7.core.cluster

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.generic.Completed
import js7.base.problem.{Checked, Problem}
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.common.akkahttp.StandardMarshallers._
import js7.data.cluster.ClusterState
import js7.data.master.MasterId
import io.circe.JsonObject
import monix.eval.Task
import monix.execution.Scheduler

trait MastersClusterRoute
{
  protected def clusterWatchRegister: ClusterWatchRegister
  protected def scheduler: Scheduler

  private implicit def implicitScheduler = scheduler

  protected final def masterClusterRoute(masterId: MasterId): Route =
    pathEnd {
      post {
        entity(as[ClusterWatchMessage]) {
          case ClusterWatchEvents(from, events, clusterState, force) =>
            complete(
              clusterWatchRegister(masterId)
                .flatMap(_.applyEvents(from, events, clusterState, force))
                .map(_.map((_: Completed) => JsonObject.empty))
                .runToFuture)

          case ClusterWatchHeartbeat(from, clusterState) =>
            complete(
              clusterWatchRegister(masterId)
                .flatMap(_.heartbeat(from, clusterState))
                .map(_.map((_: Completed) => JsonObject.empty))
                .runToFuture)
        }
      } ~
      get {
        complete(
          clusterWatchRegister.tryRead(masterId)
            .flatMap[Checked[ClusterState]] {
              case None =>
                Task.pure(Problem(s"No ClusterState registered for MasterId '$masterId'"))
              case Some(clusterWatch) =>
                clusterWatch.get
            }.runToFuture)
      }
    }
}
