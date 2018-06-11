package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal
import akka.http.scaladsl.server.{Directive0, ExceptionHandler, RejectionHandler}
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.exceptions.PublicException
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives._
import com.sos.jobscheduler.common.log.LogLevel
import com.sos.jobscheduler.common.log.LogLevel._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.parser.{parse ⇒ parseJson}
import java.time.Duration
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
trait WebLogDirectives {

  protected def config: Config
  protected def actorSystem: ActorSystem

  private lazy val logLevel = LogLevel(config.getString("jobscheduler.webserver.log.level"))
  private lazy val respondWithException = config.getBoolean("jobscheduler.webserver.verbose-error-messages")
  private lazy val hasRemoteAddress = actorSystem.settings.config.getBoolean("akka.http.server.remote-address-header")

  def handleErrorAndLog(userId: Option[UserId] = None): Directive0 =
    mapInnerRoute { inner ⇒
      webLog(userId) {
        handleError {
          inner
        }
      }
    }

  private def webLog(userId: Option[UserId]): Directive0 = extractRequest.flatMap { request ⇒
    val start = System.nanoTime
    mapResponse { response ⇒
      log(request, response, logLevel, userId, System.nanoTime - start, hasRemoteAddress = hasRemoteAddress)
      response
    }
  }

  private val exceptionHandler = ExceptionHandler {
    case e: HttpStatusCodeException ⇒
      complete((e.statusCode, e.problem))

    case e: PublicException ⇒
      extractRequest { request ⇒
        webLogger.warn(toLogMessage(request, e), e)
        complete((BadRequest, Problem.fromEagerThrowable(e)))
      }

    case e ⇒
      extractRequest { request ⇒
        webLogger.warn(toLogMessage(request, e), e)
        if (respondWithException)
          complete((InternalServerError, Problem.fromEagerThrowable(e)))
        else
          complete(InternalServerError)
      }
  }

  def handleError: Directive0 = {
    mapInnerRoute { inner ⇒
      extractSettings { implicit routingSettings ⇒
        seal {
          handleExceptions(exceptionHandler) {
            handleRejections(RejectionHandler.default) {
              inner
            }
          }
        }
      }
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

  private def toLogMessage(request: HttpRequest, throwable: Throwable) =
    s"Error while handling ${request.method.value} ${request.uri}: ${throwable.toStringWithCauses}"
}

object WebLogDirectives {
  private val webLogger = Logger("Web")

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
