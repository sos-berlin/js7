package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives._
import com.sos.jobscheduler.common.log.LogLevel
import com.sos.jobscheduler.common.log.LogLevel._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.parser.{parse ⇒ parseJson}
import java.lang.System.nanoTime
import java.time.Duration
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
trait WebLogDirectives extends ExceptionHandling {

  protected def config: Config
  protected def actorSystem: ActorSystem

  private lazy val logLevel = LogLevel(config.getString("jobscheduler.webserver.log.level"))
  private lazy val hasRemoteAddress = actorSystem.settings.config.getBoolean("akka.http.server.remote-address-header")

  protected def webLog(userId: Option[UserId]): Directive0 =
    mapInnerRoute { inner ⇒
      webLogOnly(userId) {
        seal {
          inner
        }
      }
    }

  private def webLogOnly(userId: Option[UserId]): Directive0 =
    extractRequest flatMap { request ⇒
      val start = nanoTime
      mapResponse { response ⇒
        log(request, response, logLevel, userId, nanoTime - start, hasRemoteAddress = hasRemoteAddress)
        response
      }
    }

  private def log(request: HttpRequest, response: HttpResponse, logLevel: LogLevel, userId: Option[UserId], nanos: Long, hasRemoteAddress: Boolean)(): Unit = {
    val isFailure = response.status.isFailure
    webLogger.log(
      if (isFailure) Warn else logLevel,
      requestResponseToLine(request, response, userId, nanos, hasRemoteAddress = hasRemoteAddress))
  }

  private def requestResponseToLine(request: HttpRequest, response: HttpResponse, userId: Option[UserId], nanos: Long, hasRemoteAddress: Boolean) = {
    val sb = new StringBuilder(200)
    sb.append(response.status.intValue)
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
    if (response.status.isFailure)
      response.entity match {  // Try to extract error message
        case entity @ HttpEntity.Strict(`text/plain(UTF-8)`, _) ⇒
          appendQuotedString(sb, entity.data.utf8String take 210 takeWhile { c ⇒ !c.isControl } truncateWithEllipsis 200)

        case entity @ HttpEntity.Strict(`application/json`, _) ⇒
          parseJson(entity.data.utf8String) flatMap (_.as[Problem]) match {
            case Left(_) ⇒ appendQuotedString(sb, response.status.reason)
            case Right(problem) ⇒ appendQuotedString(sb, problem.toString)
          }

        case _ ⇒
          appendQuotedString(sb, response.status.reason)
      }
    else response.entity match {
      case entity: HttpEntity.Strict ⇒
        sb.append(entity.data.length)
      case _ ⇒
        sb.append("STREAM")
    }
    sb.append(' ')
    sb.append(Duration.ofNanos(nanos).pretty)
    sb.toString
  }
}

object WebLogDirectives {
  private val webLogger = Logger("web.log")

  val TestConfig = ConfigFactory.parseMap(Map(
    "jobscheduler.webserver.log.level" → "debug",
    "jobscheduler.webserver.verbose-error-messages" → true.toString)
    .asJava)

  def apply(config: Config, actorSystem: ActorSystem): WebLogDirectives = {
    val c = config
    val a = actorSystem
    new WebLogDirectives {
      def config = c
      def actorSystem = a
    }
  }

  private def appendQuotedString(sb: StringBuilder, string: String) = {
    sb.append('"')
    string foreach {
      case '"' ⇒ sb.append('\\').append('"')
      case ch ⇒ sb.append(ch)
    }
    sb.append('"')
  }
}
