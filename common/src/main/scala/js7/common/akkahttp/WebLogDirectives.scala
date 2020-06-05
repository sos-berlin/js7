package js7.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse, StatusCode}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives._
import js7.base.auth.UserId
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.Strings._
import js7.common.akkahttp.WebLogDirectives._
import js7.common.log.LogLevel
import js7.common.log.LogLevel._
import js7.common.scalautil.Logger
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

  private lazy val logLevel = LogLevel(config.getString("js7.webserver.log.level"))
  private lazy val errorLogLevel = LogLevel(config.getString("js7.webserver.log.error-level"))
  private lazy val internalServerErrrorLevel = LogLevel(config.getString("js7.webserver.log.500-level"))
  private lazy val logResponse = actorSystem.settings.config.getBoolean("js7.webserver.log.response")
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
      case status if status.intValue == 500 => internalServerErrrorLevel
      case _ => errorLogLevel
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
            appendQuotedString(sb, truncated + ((truncated.length < string.length) ?: "..."))

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
  private val webLogger = Logger("js7.web.log")

  val TestConfig = ConfigFactory.parseString("""
     |js7.webserver.log.level = debug
     |js7.webserver.log.error-level = debug
     |js7.webserver.log.500-level = info
     |js7.webserver.log.duration = off
     |js7.webserver.verbose-error-messages = true
     |js7.webserver.shutdown-timeout = 10s""")

  private def appendQuotedString(sb: StringBuilder, string: String) = {
    sb.append('"')
    string foreach {
      case '"' => sb.append('\\').append('"')
      case ch => sb.append(ch)
    }
    sb.append('"')
  }
}
