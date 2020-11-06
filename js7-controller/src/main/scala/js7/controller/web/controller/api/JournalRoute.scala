package js7.controller.web.controller.api

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.base.auth.ValidUserPermission
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.FutureCompletion
import js7.base.utils.FutureCompletion.syntax._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.AkkaHttpServerUtils.accept
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.event.{EventWatch, PositionAnd}
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport._
import js7.common.scalautil.Logger
import js7.common.time.JavaTimeConverters._
import js7.controller.web.common.ControllerRouteProvider
import js7.controller.web.controller.api.JournalRoute._
import js7.data.event.{EventId, JournalSeparators}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration

/** Returns the content of an old or currently written journal file as a live stream.
  * Additional to EventRoute this web service returns the complete file including
  * - JournalHeader
  * - JournalSeparators
  * - Snapshots section
  * - Events (including transaction separators)
  */
trait JournalRoute extends ControllerRouteProvider
{
  protected def eventWatch: EventWatch
  protected def scheduler: Scheduler

  private implicit def implicitScheduler = scheduler

  private lazy val whenShuttingDownCompletion = new FutureCompletion(whenShuttingDown)

  private lazy val defaultJsonSeqChunkTimeout = config.getDuration("js7.web.server.services.event.streaming.chunk-timeout")
    .toFiniteDuration

  protected final lazy val journalRoute: Route =
    get {
      pathEnd {
        handleExceptions(exceptionHandler) {
          authorizedUser(ValidUserPermission) { _ =>
            parameter("file".as[EventId].?) { maybeFileEventId =>
              parameter("position".as[Long].?) { maybePosition =>
                parameter("timeout" ? defaultJsonSeqChunkTimeout) { timeout =>
                  parameter("markEOF" ? false) { markEOF =>
                    parameter("heartbeat".as[FiniteDuration].?) { heartbeat =>
                      parameter("return" ? "") { returnType =>
                        accept(JournalContentType) {
                          complete(
                            Task.pure(parseReturnAckParameter(returnType)).flatMapT(returnAck =>
                              eventWatch.observeFile(fileEventId = maybeFileEventId, position = maybePosition, timeout,
                                                      markEOF = markEOF, onlyAcks = returnAck)
                                .flatMapT { observable =>
                                  val f = if (returnAck) toLength _ else toContent _
                                  Task.pure(Right(
                                    HttpEntity(
                                      JournalContentType,
                                      observable.takeUntilCompletedAndDo(whenShuttingDownCompletion)(_ =>
                                        Task { logger.debug("whenShuttingDown completed") }
                                      ) .map(f)
                                        .pipeIf(heartbeat.isDefined)(_
                                          .insertHeartbeatsOnSlowUpstream(heartbeat.get, HeartbeatMarker))
                                        .map(_.toByteString)
                                        .toAkkaSourceForHttpResponse)))
                                  })
                          .runToFuture)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

  private def toContent(o: PositionAnd[ByteArray]) =
    o.value

  private def toLength(o: PositionAnd[ByteArray]): ByteArray =
    if (o.value != EndOfJournalFileMarker)
      ByteArray(o.position.toString + '\n')
    else
      o.value
}

object JournalRoute
{
  private val logger = Logger(getClass)
  private val JournalContentType = `application/x-ndjson`
  private val EndOfJournalFileMarker = JournalSeparators.EndOfJournalFileMarker.toByteArray
  private val HeartbeatMarker = JournalSeparators.HeartbeatMarker.toByteArray

  private def parseReturnAckParameter(returnType: String): Checked[Boolean] =
    returnType match {
      case "" => Right(false)
      case "ack" => Right(true)
      case _ => Left(Problem(s"Invalid parameter: return=$returnType"))
    }
}
