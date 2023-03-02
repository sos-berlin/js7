package js7.core.cluster.watch

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.JsonObject
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkahttp.AkkaHttpServerUtils.{completeTask, extractJs7RequestId}
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.StandardMarshallers.*
import js7.data.cluster.ClusterWatchRequest
import js7.data.controller.ControllerId
import monix.eval.Task
import monix.execution.Scheduler

trait ClusterWatchRoute
{
  protected type Session = ClusterWatchSession
  protected def clusterWatchRegister: ClusterWatchRegister
  protected def scheduler: Scheduler

  private implicit def implicitScheduler: Scheduler = scheduler

  protected final def clusterWatchRouteFor(controllerId: ControllerId, session: ClusterWatchSession): Route =
    post {
      entity(as[ClusterWatchRequest]) { msg =>
        extractJs7RequestId(requestNumber =>
          completeTask(
            session.withRequestId(requestNumber)(
              clusterWatchRegister(controllerId)
                .flatMap(clusterWatch =>
                  Task(clusterWatch.processRequest(msg)))
                .rightAs(JsonObject.empty))))
      }
    } ~
    get {
      completeTask(
        clusterWatchRegister.tryRead(controllerId)
          .map {
            case None =>
              Left(Problem(s"No ClusterState registered for '$controllerId'"))
            case Some(clusterWatch) =>
              Right(clusterWatch.clusterState())
          })
    }
}
