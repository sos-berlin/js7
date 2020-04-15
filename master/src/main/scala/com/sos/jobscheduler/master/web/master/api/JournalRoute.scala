package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.ByteString
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.base.monixutils.MonixBase.syntax._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichAny
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.accept
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.event.{EventWatch, PositionAnd}
import com.sos.jobscheduler.common.http.AkkaHttpUtils.AkkaByteVector
import com.sos.jobscheduler.common.http.JsonStreamingSupport.`application/x-ndjson`
import com.sos.jobscheduler.common.http.StreamingSupport._
import com.sos.jobscheduler.common.time.JavaTimeConverters._
import com.sos.jobscheduler.core.event.journal.data.JournalSeparators
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import com.sos.jobscheduler.master.web.master.api.JournalRoute._
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration

/** Returns the content of an old or currently written journal file as a live stream.
  * Additional to EventRoute this web service returns the complete file including
  * - JournalHeader
  * - JournalSeparators
  * - Snapshots section
  * - Events (including transaction separators)
  */
trait JournalRoute extends MasterRouteProvider
{
  protected def eventWatch: EventWatch
  protected def scheduler: Scheduler

  private lazy val defaultJsonSeqChunkTimeout = config.getDuration("jobscheduler.webserver.services.event.streaming.chunk-timeout")
    .toFiniteDuration
  private implicit def implicitScheduler = scheduler

  protected final def journalRoute: Route =
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
                            for {
                              returnLength <- parseReturnParameter(returnType)
                              observable <- eventWatch.observeFile(fileEventId = maybeFileEventId, position = maybePosition, timeout,
                                markEOF = markEOF, onlyLastOfChunk = returnLength)
                            } yield {
                              val f = if (returnLength) toLength _ else toContent _
                              HttpEntity(JournalContentType,
                                logAkkaStreamErrorToWebLogAndIgnore(
                                  observable
                                    .takeWhile(_ => !isShuttingDown)
                                    .map(f)
                                    .pipe(o => heartbeat.fold(o)(o.beatOnSlowUpstream(_, HeartbeatMarker)))
                                    .toAkkaSource))
                            })
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

  private def toContent(o: PositionAnd[ByteString]) =
    o.value

  private def toLength(o: PositionAnd[ByteString]): ByteString =
    if (o.value != EndOfJournalFileMarker)
      ByteString(o.position.toString + '\n')
    else
      o.value
}

object JournalRoute
{
  private val JournalContentType = `application/x-ndjson`
  private val HeartbeatMarker = JournalSeparators.HeartbeatMarker.toByteString
  private val EndOfJournalFileMarker = JournalSeparators.EndOfJournalFileMarker.toByteString

  private def parseReturnParameter(returnType: String): Checked[Boolean] =
    returnType match {
      case "" => Right(false)
      case "length" => Right(true)
      case _ => Left(Problem(s"Invalid parameter: return=$returnType"))
    }
}
