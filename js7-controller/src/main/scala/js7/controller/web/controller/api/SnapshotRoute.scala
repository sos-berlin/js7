package js7.controller.web.controller.api

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.HttpEntity.Chunk
import akka.http.scaladsl.server.Directives.{complete, get, pathEndOrSingleSlash}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.ParameterDirectives._
import akka.util.ByteString
import io.circe.syntax._
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.data.ByteSequence.ops._
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.completeTask
import js7.common.akkahttp.ByteSequenceChunkerObservable.syntax._
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport.AkkaObservable
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.SnapshotRoute._
import js7.data.Problems.SnapshotForUnknownEventIdProblem
import js7.data.controller.ControllerState
import js7.data.event.EventId
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.reactive.Observable

trait SnapshotRoute extends ControllerRouteProvider
{
  protected def controllerState: Task[Checked[ControllerState]]
  protected def eventWatch: FileEventWatch
  protected def controllerConfiguration: ControllerConfiguration

  private implicit def implicitScheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  final lazy val snapshotRoute = filteredSnapshotRoute(identity)

  final def filteredSnapshotRoute(filter: SnapshotFilter): Route =
    get {
      authorizedUser(ValidUserPermission) { _ =>
        pathEndOrSingleSlash {
          parameter("eventId".as[Long].?) {
            case None => currentSnapshot(filter)
            case Some(eventId) => historicSnapshot(eventId)
          }
        }
      }
    }

  private def currentSnapshot(filter: SnapshotFilter): Route =
    completeTask(
      for (checkedState <- controllerState) yield
        for (state <- checkedState) yield
          snapshotToHttpEntity(state, filter))

  private def historicSnapshot(eventId: EventId): Route =
    complete {
      val checked = eventWatch.rawSnapshotAfter(after = eventId) match {
        case None =>
          Left(SnapshotForUnknownEventIdProblem(eventId))

        case Some(observable) =>
          Right(HttpEntity.Chunked(
            `application/x-ndjson`,
            observable
              .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
                Task { logger.debug("whenShuttingDown completed") })
              .map(_.toByteString)
              .chunk(chunkSize)  // TODO Maybe fill-up chunks
              .map(Chunk(_))
              .toAkkaSourceForHttpResponse))
      }
      checked
    }

  private def snapshotToHttpEntity(state: ControllerState, filter: SnapshotFilter) =
    HttpEntity.Chunked(
      `application/x-ndjson`,
      filter(state.toSnapshotObservable)
        .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
          Task { logger.debug("whenShuttingDown completed") })
        .mapParallelBatch()(_
          .asJson(ControllerState.snapshotObjectJsonCodec)
          .toByteSequence[ByteString]
          .concat(LF)
          .chunk(chunkSize))
        .flatMap(Observable.fromIterable)
        .map(Chunk(_))
        .toAkkaSourceForHttpResponse)
}

object SnapshotRoute
{
  type SnapshotFilter = Observable[Any] => Observable[Any]

  private val logger = Logger(getClass)
  private val LF = ByteString("\n")
}
