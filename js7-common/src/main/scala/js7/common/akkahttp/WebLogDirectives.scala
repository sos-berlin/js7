package js7.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.headers.{Referer, `User-Agent`}
import akka.http.scaladsl.model.{AttributeKeys, HttpEntity, HttpHeader, HttpRequest, HttpResponse, StatusCode}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives._
import com.typesafe.config.Config
import io.circe.parser.{parse => parseJson}
import java.lang.System.nanoTime
import js7.base.auth.SessionToken
import js7.base.configutils.Configs._
import js7.base.log.LogLevel.syntax._
import js7.base.log.{LogLevel, Logger}
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.common.akkahttp.WebLogDirectives._
import js7.common.http.AkkaHttpClient.{`x-js7-request-id`, `x-js7-session`}
import scala.concurrent.duration._
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
  private lazy val hasRemoteAddress = actorSystem.settings.config.getBoolean("akka.http.server.remote-address-attribute")

  protected def webLog: Directive0 =
    mapInnerRoute { inner =>
      webLogOnly {
        seal {
          inner
        }
      }
    }

  private def webLogOnly: Directive0 =
    if (!logRequest && !logResponse)
      pass
    else
      extractRequest flatMap { request =>
        if (logRequest || webLogger.underlying.isTraceEnabled) {
          log(request, None, if (logRequest) logLevel else LogLevel.Trace,
            nanos = 0, hasRemoteAddress = hasRemoteAddress)
        }
        if (logResponse) {
          val start = nanoTime
          mapResponse { response =>
            log(request, Some(response), statusToLogLevel(response.status),
              nanoTime - start, hasRemoteAddress = hasRemoteAddress)
            response
          }
        } else
          pass
      }

  private def log(request: HttpRequest, response: Option[HttpResponse], logLevel: LogLevel,
    nanos: Long, hasRemoteAddress: Boolean): Unit
  =
    webLogger.log(
      logLevel,
      requestResponseToLine(request, response, nanos, hasRemoteAddress = hasRemoteAddress))

  private def statusToLogLevel(statusCode: StatusCode): LogLevel =
    statusCode match {
      case status if status.intValue < 400 => logLevel
      case status if status.intValue == 500 => internalServerErrorLevel
      case _ => errorLogLevel
    }

  private def requestResponseToLine(request: HttpRequest, maybeResponse: Option[HttpResponse],
    nanos: Long, hasRemoteAddress: Boolean)
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
      case _ => sb.append("in ")
    }
    sb.append(' ')
    // Let SessionToken and request number look like in AkkaHttpClient
    sb.append(request.headers
      .collectFirst {
        case `x-js7-session`(secret) => SessionToken.stringToShort(secret)
      }
      .getOrElse(SessionToken.PrefixChar))
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
            sb.append(entity.data.length)
          case _ =>
            sb.append(" STREAM")
        }
        sb.append(' ')
        sb.append(nanos.nanoseconds.pretty)
    }
    sb.toString
  }
}

object WebLogDirectives
{
  private val webLogger = Logger("js7.web.log")

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
