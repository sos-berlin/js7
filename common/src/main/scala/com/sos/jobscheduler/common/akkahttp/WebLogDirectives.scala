package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse, StatusCode}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Strings._
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives._
import com.sos.jobscheduler.common.log.LogLevel
import com.sos.jobscheduler.common.log.LogLevel._
import com.sos.jobscheduler.common.scalautil.Logger
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.parser.{parse => parseJson}
import java.lang.System.nanoTime
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait WebLogDirectives extends ExceptionHandling
{
  protected def config: Config
  protected def actorSystem: ActorSystem

  private lazy val logLevel = LogLevel(config.getString("jobscheduler.webserver.log.level"))
  private lazy val log4xxLevel = LogLevel(config.getString("jobscheduler.webserver.log.4xx-level"))
  private lazy val log5xxLevel = LogLevel(config.getString("jobscheduler.webserver.log.5xx-level"))
  private lazy val logResponse = actorSystem.settings.config.getBoolean("jobscheduler.webserver.log.response")
  private lazy val hasRemoteAddress = actorSystem.settings.config.getBoolean("akka.http.server.remote-address-header")

  protected def webLog(userId: Option[UserId]): Directive0 =
    mapInnerRoute { inner =>
      webLogOnly(userId) {
        seal {
          inner
        }
      }
    }

  private def webLogOnly(userId: Option[UserId]): Directive0 =
    extractRequest flatMap { request =>
      if (logResponse) {
        val start = nanoTime
        mapResponse { response =>
          log(request, Some(response), statusToLogLevel(response.status),
            userId, nanoTime - start, hasRemoteAddress = hasRemoteAddress)
          response
        }
      } else {
          log(request, None, logLevel, userId, nanos = Long.MinValue, hasRemoteAddress = hasRemoteAddress)
          pass
        }
    }

  private def log(request: HttpRequest, response: Option[HttpResponse], logLevel: LogLevel,
    userId: Option[UserId], nanos: Long, hasRemoteAddress: Boolean): Unit
  =
    webLogger.log(
      logLevel,
      requestResponseToLine(request, response, userId, nanos, hasRemoteAddress = hasRemoteAddress))

  private def statusToLogLevel(statusCode: StatusCode): LogLevel =
    statusCode match {
      case status if status.intValue < 400 => logLevel
      case status if status.intValue < 500 => log4xxLevel
      case _ => log5xxLevel
    }

  private def requestResponseToLine(request: HttpRequest, responseOption: Option[HttpResponse],
    userId: Option[UserId], nanos: Long, hasRemoteAddress: Boolean)
  = {
    val sb = new StringBuilder(200)
    for (response <- responseOption) sb.append(response.status.intValue)
    //val remoteAddress = (hasRemoteAddress option (request.header[`Remote-Address`] map { _.address })).flatten getOrElse RemoteAddress.Unknown
    //sb.append(' ')
    //sb.append(remoteAddress.toOption map { _.getHostAddress } getOrElse "-")
    sb.append(' ')
    sb.append(userId.fold("-")(_.string))
    sb.append(' ')
    sb.append(request.method.value)
    sb.append(' ')
    sb.append(request.uri)
    sb.append(' ')
    for (response <- responseOption) {
      if (response.status.isFailure)
        response.entity match {  // Try to extract error message
          case entity @ HttpEntity.Strict(`text/plain(UTF-8)`, _) =>
            val string = entity.data.utf8String
            val truncated = string.take(201).reverseDropWhile(_ == '\n').map(c => if (c.isControl) '·' else c)
            appendQuotedString(sb, truncated + ((truncated.length < string.length) ?? "..."))

          case entity @ HttpEntity.Strict(`application/json`, _) =>
            parseJson(entity.data.utf8String) flatMap (_.as[Problem]) match {
              case Left(_) => appendQuotedString(sb, response.status.reason)
              case Right(problem) => appendQuotedString(sb, problem.toString)
            }

          case _ =>
            appendQuotedString(sb, response.status.reason)
        }
      else response.entity match {
        case entity: HttpEntity.Strict =>
          sb.append(entity.data.length)
        case _ =>
          sb.append("STREAM")
      }
      sb.append(' ')
      sb.append(nanos.nanoseconds.pretty)
    }
    sb.toString
  }
}

object WebLogDirectives
{
  private val webLogger = Logger("jobscheduler.web.log")

  val TestConfig = ConfigFactory.parseString("""
     |jobscheduler.webserver.log.level = debug
     |jobscheduler.webserver.log.4xx-level = debug
     |jobscheduler.webserver.log.5xx-level = info
     |jobscheduler.webserver.log.duration = off
     |jobscheduler.webserver.verbose-error-messages = true
     |jobscheduler.webserver.shutdown-timeout = 10s""")

  private def appendQuotedString(sb: StringBuilder, string: String) = {
    sb.append('"')
    string foreach {
      case '"' => sb.append('\\').append('"')
      case ch => sb.append(ch)
    }
    sb.append('"')
  }
}
