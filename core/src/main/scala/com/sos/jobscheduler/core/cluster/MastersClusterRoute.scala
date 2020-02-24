package com.sos.jobscheduler.core.cluster

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.master.MasterId
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
          case ClusterWatchEvents(from, events, clusterState) =>
            complete(
              clusterWatchRegister(masterId)
                .flatMap(_.applyEvents(from, events, clusterState))
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
