package js7.journal.web

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.base.data.ByteArray
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkahttp.AkkaHttpServerUtils.accept
import js7.common.akkahttp.ByteSequenceChunkerObservable.syntax.*
import js7.common.akkahttp.StandardMarshallers.*
import js7.common.akkahttp.web.session.RouteProvider
import js7.common.akkautils.ByteStrings.syntax.*
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport.AkkaObservable
import js7.common.jsonseq.PositionAnd
import js7.data.event.JournalSeparators.{EndOfJournalFileMarker, HeartbeatMarker}
import js7.data.event.{EventId, JournalPosition}
import js7.journal.watch.FileEventWatch
import js7.journal.web.JournalRoute.*
import monix.eval.Task
import monix.execution.Scheduler
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
trait JournalRoute extends RouteProvider
{
  protected def eventWatch: FileEventWatch

  private implicit def implicitScheduler: Scheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)
  private lazy val defaultJsonSeqChunkTimeout = config
    .getDuration("js7.web.server.services.event.streaming.chunk-timeout")
    .toFiniteDuration

  protected final def journalRoute: Route =
    get {
      pathEnd {
        handleExceptions(exceptionHandler) {
          authorizedUser(ValidUserPermission) { _ =>
            parameter(
              "file".as[EventId].?,
              "position".as[Long].?,
              "timeout" ? defaultJsonSeqChunkTimeout,
              "markEOF" ? false,
              "heartbeat".as[FiniteDuration].?,
              "return" ? ""
            ) { (maybeFileEventId, maybePosition, timeout, markEOF, heartbeat, returnType) =>
              accept(JournalContentType) {
                complete(Task
                  .pure((maybeFileEventId, maybePosition) match {
                    case (None, None) => eventWatch.journalPosition  // Convenient for manual tests
                    case (Some(f), Some(p)) => Right(JournalPosition(f, p))
                    case _ => Left(Problem("Missing one of the arguments: file, position, eventId"))
                  })
                  .flatMapT(journalPosition => Task
                    .pure(parseReturnAckParameter(returnType))
                    .flatMapT(returnAck =>
                      eventWatch
                        .observeFile(journalPosition, timeout,
                          markEOF = markEOF, onlyAcks = returnAck)
                        .map(_.map(observable =>
                          HttpEntity.Chunked(
                            JournalContentType,
                            observable
                              .takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ => Task {
                                logger.debug("whenShuttingDown completed")
                              })
                              .map(if (returnAck) toLength else toContent)
                              .pipeIf(heartbeat.isDefined)(_
                                .insertHeartbeatsOnSlowUpstream(heartbeat.get, HeartbeatMarker))
                              .map(_.toByteString)
                              .chunk(chunkSize)
                              .map(HttpEntity.Chunk(_))
                              .toAkkaSourceForHttpResponse)))))
                  .runToFuture)
              }
            }
          }
        }
      }
    }
}

object JournalRoute
{
  private val logger = Logger[this.type]
  private val JournalContentType = `application/x-ndjson`

  private def parseReturnAckParameter(returnType: String): Checked[Boolean] =
    returnType match {
      case "" => Right(false)
      case "ack" => Right(true)
      case _ => Left(Problem(s"Invalid parameter: return=$returnType"))
    }

  private def toContent(o: PositionAnd[ByteArray]) =
    o.value

  private def toLength(o: PositionAnd[ByteArray]): ByteArray =
    if (o.value != EndOfJournalFileMarker)
      ByteArray(o.position.toString + '\n')
    else
      o.value
}
