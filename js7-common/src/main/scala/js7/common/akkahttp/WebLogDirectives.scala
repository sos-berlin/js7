package js7.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.headers.{Referer, `User-Agent`}
import akka.http.scaladsl.model.{AttributeKey, AttributeKeys, HttpEntity, HttpHeader, HttpRequest, HttpResponse, StatusCode}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{Directive0, Route}
import com.typesafe.config.Config
import io.circe.parser.parse as parseJson
import java.lang.System.nanoTime
import js7.base.auth.SessionToken
import js7.base.configutils.Configs.*
import js7.base.log.LogLevel.syntax.*
import js7.base.log.{CorrelId, LogLevel, Logger}
import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.akkahttp.WebLogDirectives.*
import js7.common.http.AkkaHttpClient.{`x-js7-correlation-id`, `x-js7-request-id`, `x-js7-session`}
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait WebLogDirectives extends ExceptionHandling
{
  protected def config: Config
  protected def actorSystem: ActorSystem

  private lazy val logLevel = LogLevel(config.getString("js7.web.server.log.level"))
  private lazy val errorLogLevel = LogLevel(config.getString("js7.web.server.log.error-level"))
  private lazy val internalServerErrorLevel = LogLevel(config.getString("js7.web.server.log.500-level"))
  private lazy val logRequest = actorSystem.settings.config.getBoolean("js7.web.server.log.request")
  private lazy val logResponse = actorSystem.settings.config.getBoolean("js7.web.server.log.response")
  //private lazy val hasRemoteAddress = actorSystem.settings.config.getBoolean("akka.http.server.remote-address-attribute")

  protected def webLog: Directive0 =
    mapInnerRoute { inner =>
      extractRequest { request =>
        val correlId = getCorrelId(request)
        setCorrelIdAttribute(correlId) {
          webLogOnly(request, correlId) {
            seal {
              inner
            }
          }
        }
      }
    }

  private def getCorrelId(request: HttpRequest): CorrelId =
    if (!CorrelId.isEnabled)
      CorrelId.empty
    else
      //request.header[`x-js7-correlation-id`].fold(CorrelId.generate())(_.correlId)
      request.headers
        .find(_.is(`x-js7-correlation-id`.lowercaseName))
        .flatMap(h => CorrelId.checked(h.value).toOption)
        .getOrElse(CorrelId.generate())

  private def setCorrelIdAttribute(correlId: CorrelId)(route: Route): Route =
    if (!CorrelId.isEnabled)
      route
    else
      mapRequest(_.addAttribute(CorrelIdAttributeKey, correlId))(route)

  private def webLogOnly(request: HttpRequest, correlId: CorrelId): Directive0 =
    if (!logRequest && !logResponse)
      pass
    else {
      if (logRequest || webLogger.underlying.isTraceEnabled) {
        log(request, None, correlId, if (logRequest) logLevel else LogLevel.Trace, nanos = 0)
      }
      if (logResponse) {
        val start = nanoTime
        mapResponse { response =>
          log(request, Some(response), correlId, statusToLogLevel(response.status), nanoTime - start)
          meterTime(request, response, correlId, start)
        }
      } else
        pass
    }

  private def meterTime(request: HttpRequest, response: HttpResponse, correlId: CorrelId, start: Long)
  : HttpResponse = {
    val since = now
    var chunkCount = 0L
    var byteCount = 0L
    response.entity match {
      case entity: HttpEntity.Chunked =>
        response.withEntity(
          entity.copy(chunks =
            entity.chunks.wireTap { part =>
              chunkCount += 1
              byteCount += part.data.size
            }
            .watchTermination() { (mat, future) =>
              future.onComplete { tried =>
                log(request, Some(response), correlId, logLevel, nanoTime - start,
                  streamSuffix = // Timing of the stream, after response header has been sent
                    chunkCount.toString + " chunks, " +
                    bytesPerSecondString(since.elapsed, byteCount) +
                      tried.fold(t => " " + t.toStringWithCauses, _ => ""))
              }(actorSystem.dispatcher)
              mat
            }))
      case _ => response
    }
  }

  private def log(request: HttpRequest, response: Option[HttpResponse],
    correlId: CorrelId, logLevel: LogLevel, nanos: Long, streamSuffix: String = ""): Unit
  =
    correlId.bind {
      webLogger.log(
        logLevel,
        requestResponseToLine(request, response, nanos, streamSuffix))
    }

  private def statusToLogLevel(statusCode: StatusCode): LogLevel =
    statusCode match {
      case status if status.intValue < 400 => logLevel
      case status if status.intValue == 500 => internalServerErrorLevel
      case _ => errorLogLevel
    }

  private def requestResponseToLine(request: HttpRequest, maybeResponse: Option[HttpResponse],
    nanos: Long, streamSuffix: String)
  = {
    val sb = new StringBuilder(256)

    def appendHeader[A >: Null <: HttpHeader: ClassTag](): Unit = {
      request.header[A] match {
        case None => sb.append(" -")
        case Some(h) => appendQuotedString(sb, h.value)
      }
    }

    maybeResponse match {
      case Some(response) => sb.append(response.status.intValue)
      case _ => sb.append("-->")
    }
    sb.append(' ')
    // Let SessionToken and request number look like in AkkaHttpClient
    sb.append(request.headers
      .collectFirst {
        case `x-js7-session`(secret) => SessionToken.stringToShort(secret)
      }
      .getOrElse("-"))
    sb.append(' ')
    sb.append(request.headers.collectFirst { case `x-js7-request-id`(id) => id } getOrElse "#")
    sb.append(' ')
    sb.append(request.attribute(AttributeKeys.remoteAddress).flatMap(_.toIP)
      .fold("-")(_.ip.getHostAddress))
    //val remoteAddress = (hasRemoteAddress option (request.header[`Remote-Address`] map { _.address })).flatten getOrElse RemoteAddress.Unknown
    //sb.append(' ')
    //sb.append(remoteAddress.toOption map { _.getHostAddress } getOrElse "-")
    sb.append(' ')
    sb.append(request.method.value)
    sb.append(' ')
    sb.append(request.uri)
    maybeResponse match {
      case None =>
        appendHeader[Referer]()
        appendHeader[`User-Agent`]()

      case Some(response) =>
        if (response.status.isFailure)
          response.entity match {  // Try to extract error message
            case entity @ HttpEntity.Strict(`text/plain(UTF-8)`, _) =>
              val string = entity.data.utf8String
              val truncated = string.take(1001).dropLastWhile(_ == '\n').map(c => if (c.isControl) '·' else c)
              appendQuotedString(sb, truncated + ((truncated.length < string.length) ?? "..."))

            case entity @ HttpEntity.Strict(`application/json`, _) =>
              parseJson(entity.data.utf8String).flatMap(_.as[Problem]) match {
                case Left(_) => appendQuotedString(sb, response.status.reason)
                case Right(problem) => appendQuotedString(sb, problem.toString)
              }

            case _ =>
              appendQuotedString(sb, response.status.reason)
          }
        else response.entity match {
          case entity: HttpEntity.Strict =>
            sb.append(' ')
            sb.append(toKBGB(entity.data.length))
            sb.append(' ')
            sb.append(nanos.nanoseconds.pretty)
          case _ =>
            if (streamSuffix.isEmpty) {
              sb.append(" STREAM... ")
              sb.append(nanos.nanoseconds.pretty)
            } else {
              sb.append(' ')
              sb.append(streamSuffix)
            }
        }
    }
    sb.toString
  }
}

object WebLogDirectives
{
  private val webLogger = Logger("js7.web.log")

  val CorrelIdAttributeKey = AttributeKey[CorrelId]("CorrelId")

  val TestConfig = config"""
    js7.web.server.log.level = debug
    js7.web.server.log.error-level = debug
    js7.web.server.log.500-level = info
    js7.web.server.log.duration = off
    js7.web.server.verbose-error-messages = true
    js7.web.server.shutdown-timeout = 10s"""

  private def appendQuotedString(sb: StringBuilder, string: String) = {
    sb.ensureCapacity(3 + string.length)
    sb.append(" \"")
    if (!string.contains('"')) {
      sb.append(string)
    } else {
      string foreach {
        case '"' => sb.append('\\').append('"')
        case ch => sb.append(ch)
      }
    }
    sb.append('"')
  }
}
