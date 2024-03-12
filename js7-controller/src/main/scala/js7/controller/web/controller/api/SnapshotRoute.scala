package js7.controller.web.controller.api

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import io.circe.Encoder
import js7.base.auth.ValidUserPermission
import js7.base.catsutils.CatsEffectExtensions.right
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.pekkohttp.PekkoHttpServerUtils.{accept, completeWithCheckedJsonStream, completeWithIOStream}
import js7.common.pekkohttp.StandardDirectives.ioRoute
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.{PekkoHttpServerUtils, StandardDirectives}
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.controller.configuration.ControllerConfiguration
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.SnapshotRoute.*
import js7.data.Problems.SnapshotForUnknownEventIdProblem
import js7.data.controller.ControllerState
import js7.data.event.EventId
import js7.journal.watch.FileEventWatch
import org.apache.pekko.http.scaladsl.server.Directives.{complete, get, pathEndOrSingleSlash}
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.http.scaladsl.server.directives.ParameterDirectives.*

trait SnapshotRoute extends ControllerRouteProvider:

  protected def controllerState: IO[Checked[ControllerState]]
  protected def eventWatch: FileEventWatch
  protected def controllerConfiguration: ControllerConfiguration

  private given IORuntime = ioRuntime

  final lazy val snapshotRoute = filteredSnapshotRoute(identity)

  final def filteredSnapshotRoute(filter: SnapshotFilter): Route =
    get:
      authorizedUser(ValidUserPermission) { _ =>
        pathEndOrSingleSlash:
          parameter("eventId".as[Long].?):
            case None => currentSnapshot(filter)
            case Some(eventId) => historicSnapshot(eventId)
      }

  private def currentSnapshot(filter: SnapshotFilter)(using IORuntime): Route =
    given Encoder[Any] = ControllerState.snapshotObjectJsonCodec
    completeWithCheckedJsonStream(chunkSize = chunkSize, prefetch = prefetch):
      controllerState.flatMapT: controllerState =>
        IO.right:
          filter(controllerState.toSnapshotStream)
            .interruptWhenF(shutdownSignaled)

  private def historicSnapshot(eventId: EventId): Route =
    eventWatch.rawSnapshotAfter(after = eventId) match
      case None =>
        complete(SnapshotForUnknownEventIdProblem(eventId))

      case Some(stream) =>
        accept(`application/x-ndjson`):
          ioRoute:
            completeWithIOStream(`application/x-ndjson`):
              stream
                .map(_.toChunk)
                .unchunks
                .chunkLimit(chunkSize)  // TODO Maybe fill-up chunks
                .map(_.toByteString)
                .interruptWhenF(shutdownSignaled)


object SnapshotRoute:
  type SnapshotFilter = Stream[IO, Any] => Stream[IO, Any]
