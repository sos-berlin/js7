package js7.controller.web.controller.api

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives.get
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.ParameterDirectives._
import akka.http.scaladsl.server.directives.PathDirectives.pathSingleSlash
import akka.util.ByteString
import io.circe.syntax._
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.ConcurrentRequestLimiter
import js7.common.akkahttp.StandardMarshallers._
import js7.common.event.EventWatch
import js7.common.http.AkkaHttpUtils.AkkaByteSequence
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport.AkkaObservable
import js7.common.scalautil.Logger
import js7.controller.configuration.ControllerConfiguration
import js7.controller.data.ControllerState
import js7.controller.problems.HistoricSnapshotServiceBusyProblem
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.SnapshotRoute._
import js7.data.Problems.SnapshotForUnknownEventIdProblem
import js7.data.event.EventId
import monix.eval.Task

trait SnapshotRoute extends ControllerRouteProvider
{
  protected def controllerState: Task[Checked[ControllerState]]
  protected def eventWatch: EventWatch
  protected def controllerConfiguration: ControllerConfiguration

  private implicit def implicitScheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  // Rebuilding state may take a long time, so we allow only one per time.
  // The client may impatiently abort and retry, overloading the server with multiple useless requests.
  private lazy val concurrentRequestsLimiter =
    new ConcurrentRequestLimiter(limit = 1, HistoricSnapshotServiceBusyProblem, timeout = 1.s, queueSize = 1)

  final lazy val snapshotRoute: Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathSingleSlash {
          parameter("eventId".as[Long].?) {
            case None => currentSnapshot
            case Some(eventId) => historicSnapshot(eventId)
          }
        }
      }
    }

  private lazy val currentSnapshot: Route =
    completeTask(
      for (checkedState <- controllerState) yield
        for (state <- checkedState) yield
          snapshotToHttpEntity(state))

  private def historicSnapshot(eventId: EventId): Route =
    concurrentRequestsLimiter(
      completeTask(Task.defer {
        eventWatch.rawSnapshotAfter(after = eventId) match {
          case None =>
            Task.pure(Left(SnapshotForUnknownEventIdProblem(eventId)))

          case Some(observable) =>
            Task.pure(Right(
              HttpEntity(
                `application/x-ndjson`,
                observable
                  .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
                    Task { logger.debug("whenShuttingDown completed") })
                  .map(_.toByteString)
                  .toAkkaSourceForHttpResponse)))
        }
      }))

  private def snapshotToHttpEntity(state: ControllerState): HttpEntity.Chunked =
    HttpEntity(
      `application/x-ndjson`,
      state.toSnapshotObservable
        .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
          Task { logger.debug("whenShuttingDown completed") })
        .mapParallelOrderedBatch()(o => ByteString(o.asJson(ControllerState.snapshotObjectJsonCodec).compactPrint) ++ LF)
        .toAkkaSourceForHttpResponse)
}

object SnapshotRoute
{
  private val logger = Logger(getClass)
  private val LF = ByteString("\n")
}
