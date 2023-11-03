package js7.controller.web.controller.api

import io.circe.syntax.*
import js7.base.auth.ValidUserPermission
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.data.ByteSequence.ops.*
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax.*
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport.PekkoObservable
import js7.common.pekkohttp.ByteSequenceChunkerObservable.syntax.*
import js7.common.pekkohttp.PekkoHttpServerUtils.completeTask
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.SnapshotRoute.*
import js7.data.Problems.SnapshotForUnknownEventIdProblem
import js7.data.controller.ControllerState
import js7.data.event.EventId
import js7.journal.watch.FileEventWatch
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.model.HttpEntity.Chunk
import org.apache.pekko.http.scaladsl.server.Directives.{complete, get, pathEndOrSingleSlash}
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.directives.ParameterDirectives.*
import org.apache.pekko.util.ByteString

trait SnapshotRoute extends ControllerRouteProvider
{
  protected def controllerState: Task[Checked[ControllerState]]
  protected def eventWatch: FileEventWatch
  protected def controllerConfiguration: ControllerConfiguration

  private implicit def implicitScheduler: Scheduler = scheduler

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
              .toPekkoSourceForHttpResponse))
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
        .toPekkoSourceForHttpResponse)
}

object SnapshotRoute
{
  type SnapshotFilter = Observable[Any] => Observable[Any]

  private val logger = Logger[this.type]
  private val LF = ByteString("\n")
}
