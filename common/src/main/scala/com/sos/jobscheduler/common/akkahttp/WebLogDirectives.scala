package com.sos.jobscheduler.common.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import akka.http.scaladsl.model.headers.{CustomHeader, `Remote-Address`}
import akka.http.scaladsl.model.{HttpEntity, HttpRequest, HttpResponse, RemoteAddress}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.LoggingMagnet
import akka.http.scaladsl.server.{Directive0, ExceptionHandler, RejectionHandler, RouteResult}
import com.sos.jobscheduler.base.exceptions.PublicException
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.{addHeader, passIf}
import com.sos.jobscheduler.common.log.LogLevel
import com.sos.jobscheduler.common.log.LogLevel._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.typesafe.config.{Config, ConfigFactory}
import java.time.Duration
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
object WebLogDirectives {
  private val webLogger = Logger("Web")

  val TestConfig = ConfigFactory.parseMap(Map(
    "jobscheduler.webserver.log.level" → "debug",
    "jobscheduler.webserver.log.elapsed-time" → false.toString,
    "jobscheduler.webserver.verbose-error-messages" → true.toString)
    .asJava)

  def handleErrorAndLog(config: Config, system: ActorSystem): Directive0 = {
    val logLevel = LogLevel(config.getString("jobscheduler.webserver.log.level"))
    val withTimestamp = config.getBoolean("jobscheduler.webserver.log.elapsed-time")
    val verboseErrorMessages = config.getBoolean("jobscheduler.webserver.verbose-error-messages")
    val hasRemoteAddress = system.settings.config.getBoolean("akka.http.server.remote-address-header")
    val exceptionHandler = ExceptionHandler {
      case e: PublicException ⇒
        extractRequest { request ⇒
          webLogger.debug(toLogMessage(request, e), e)
          complete((BadRequest, e.publicMessage + "\n"))
        }

      case e: HttpStatusCodeException ⇒
        complete((e.statusCode, e.message + "\n"))

      case e ⇒
        extractRequest { request ⇒
          webLogger.debug(toLogMessage(request, e), e)
          if (verboseErrorMessages)
            complete((InternalServerError, e.toStringWithCauses + "\n"))  // .toSimplifiedString hides combined Problems (see Problem.semigroup)
          else
            complete(InternalServerError)
        }
    }
    mapInnerRoute { inner ⇒
      (passIf(!withTimestamp) | addHeader(`X-JobScheduler-Request-Started-At`(System.nanoTime))) {
        logRequestResult(LoggingMagnet(_ ⇒ log(logLevel, hasRemoteAddress = hasRemoteAddress))) {
          handleExceptions(exceptionHandler) {
            handleRejections(RejectionHandler.default) {
              inner
            }
          }
        }
      }
    }
  }

  private def log(logLevel: LogLevel, hasRemoteAddress: Boolean)(request: HttpRequest)(routeResult: RouteResult): Unit = {
    val isFailure = PartialFunction.cond(routeResult) {
      case _: RouteResult.Rejected ⇒ true //rejected.status.isFailure
      case _: RouteResult.Complete ⇒ false
    }
    webLogger.log(
      if (isFailure) Warn else logLevel,
      requestResponseToLine(request, routeResult, hasRemoteAddress = hasRemoteAddress))
  }

  private def requestResponseToLine(request: HttpRequest, response: Any, hasRemoteAddress: Boolean) = {
    val remoteAddress = (hasRemoteAddress option (request.header[`Remote-Address`] map { _.address })).flatten getOrElse RemoteAddress.Unknown
    val sb = new StringBuilder(500)
    sb.append(remoteAddress.toOption map { _.getHostAddress } getOrElse "-")
    sb.append(" ")
    sb.append(request.method.value)
    sb.append(' ')
    sb.append(request.uri)
    sb.append(' ')
    sb.append(request.protocol.value)
    response match {
      case response: HttpResponse ⇒
        sb.append(' ')
        sb.append(response.status.intValue)
        if (response.status.isFailure) {
            sb.append(' ')
          response.entity match {
            case entity @ HttpEntity.Strict(`text/plain(UTF-8)`, _) ⇒  // Error message body is only logged if encoded as "text/plain; charset=UTF-8". Don't rely on this !!!
              sb.append(entity.data.utf8String take 200 takeWhile { c ⇒ !c.isControl } truncateWithEllipsis 200)
            case _ ⇒
              sb.append(response.status.reason)
          }
        } else {
          response.entity match {
            case entity: HttpEntity.Strict ⇒
              sb.append(' ')
              sb.append(entity.data.length)
            case _ ⇒
          }
        }
      case _ ⇒
    }
    for (`X-JobScheduler-Request-Started-At`(startedAt) ← request.header[`X-JobScheduler-Request-Started-At`]) {
      sb.append(' ')
      sb.append(Duration.ofNanos(System.nanoTime - startedAt).pretty)
    }
    sb.toString
  }

  private case class `X-JobScheduler-Request-Started-At`(nanos: Long) extends CustomHeader {
    def name = "X-JobScheduler-Request-Started-At"
    def value = nanos.toString
    def renderInRequests = false
    def renderInResponses = false
  }

  private def toLogMessage(request: HttpRequest, throwable: Throwable) =
    s"Error while handling ${request.method.value} ${request.uri}: ${throwable.toStringWithCauses}"
}
