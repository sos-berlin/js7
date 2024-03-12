package js7.journal.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.auth.ValidUserPermission
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.PekkoHttpClient
import js7.common.jsonseq.PositionAnd
import js7.common.pekkohttp.PekkoHttpServerUtils
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithCheckedStream
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.event.JournalSeparators.EndOfJournalFileMarker
import js7.data.event.{EventId, JournalPosition}
import js7.journal.watch.FileEventWatch
import js7.journal.web.JournalRoute.*
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import scala.concurrent.duration.FiniteDuration

// TODO Similar to GenericEventRoute
// Test is Controller's JournalRouteTest
/** Returns the content of an old or currently written journal file as a live stream.
  * Additional to EventRoute this web service returns the complete file including
  * - JournalHeader
  * - JournalSeparators
  * - Snapshots section
  * - Events (including transaction separators)
  */
trait JournalRoute extends RouteProvider:
  protected def eventWatch: FileEventWatch

  private given IORuntime = ioRuntime

  private lazy val defaultJsonSeqChunkTimeout = config
    .getDuration("js7.web.server.services.event.streaming.chunk-timeout")
    .toFiniteDuration

  protected final def journalRoute: Route =
    get:
      pathEnd:
        handleExceptions(exceptionHandler):
          authorizedUser(ValidUserPermission): _ =>
            parameter(
              "file".as[EventId].?,
              "position".as[Long].?,
              "timeout" ? defaultJsonSeqChunkTimeout,
              "markEOF" ? false,
              "heartbeat".as[FiniteDuration].?,
              "return" ? ""
            ): (maybeFileEventId, maybePosition, timeout, markEOF, heartbeat, returnType) =>
              (maybeFileEventId, maybePosition)
                .match
                  case (None, None) => eventWatch.journalPosition  // Convenient for manual tests
                  case (Some(f), Some(p)) => Right(JournalPosition(f, p))
                  case _ => Left(Problem("Missing one of the arguments: file, position, eventId"))
                .flatMap: journalPosition =>
                  parseReturnAckParameter(returnType).map(journalPosition -> _)
                .match
                  case Left(problem) => complete(problem)
                  case Right((journalPosition, returnAck)) =>
                    completeWithCheckedStream(JournalContentType):
                      eventWatch
                        .streamFile(journalPosition, timeout,
                          markEOF = markEOF, onlyAcks = returnAck)
                        .map(_.map(_
                          .interruptWhenF(shutdownSignaled)
                          .map(if returnAck then toLength else toContent)
                          .pipeIf(heartbeat.isDefined)(_
                            .keepAlive(heartbeat.get, IO.pure(PekkoHttpClient.HttpHeartbeatByteArray)))
                          .map(_.toChunk).unchunks
                          .chunkLimit(chunkSize)
                          .map(_.toByteString)
                          //.splitBigByteSeqs(chunkSize)
                          //FIXME ? .chunk(chunkSize) --> byteStrings.sum(_.length) <= chunkSize
                          /*.map(HttpEntity.Chunk(_))
                          .toPekkoSourceForHttpResponse*/))


object JournalRoute:
  private val JournalContentType = `application/x-ndjson`

  private def parseReturnAckParameter(returnType: String): Checked[Boolean] =
    returnType match
      case "" => Right(false)
      case "ack" => Right(true)
      case _ => Left(Problem(s"Invalid parameter: return=$returnType"))

  private def toContent(o: PositionAnd[ByteArray]) =
    o.value

  private def toLength(o: PositionAnd[ByteArray]): ByteArray =
    if o.value != EndOfJournalFileMarker then
      ByteArray(o.position.toString + '\n')
    else
      o.value
