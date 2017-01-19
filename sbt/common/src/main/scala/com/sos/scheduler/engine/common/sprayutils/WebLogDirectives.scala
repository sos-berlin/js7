package com.sos.scheduler.engine.common.sprayutils

import akka.actor.ActorContext
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.common.log.LogLevel
import com.sos.scheduler.engine.common.log.LogLevel._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.{addHeader, passIf}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.typesafe.config.Config
import java.time.Duration
import spray.http.ContentTypes.`text/plain(UTF-8)`
import spray.http.HttpHeaders.`Remote-Address`
import spray.http.{HttpEntity, HttpHeader, HttpRequest, HttpResponse, RemoteAddress, Rendering}
import spray.routing.Directives._
import spray.routing._
import spray.routing.directives.LoggingMagnet

/**
  * @author Joacim Zschimmer
  */
object WebLogDirectives {
  private val webLogger = Logger("Web")

  def handleErrorAndLog(subConfig: Config)(implicit routingSettings: RoutingSettings, actorContext: ActorContext): Directive0 = {
    val logLevel = LogLevel(subConfig.getString("log.level"))
    val withTimestamp = subConfig.getBoolean("log.elapsed-time")
    val hasRemoteAddress = actorContext.system.settings.config.getBoolean("spray.can.server.remote-address-header")
    mapInnerRoute { inner ⇒
      (passIf(!withTimestamp) | addHeader(`X-JobScheduler-Request-Started-At`(System.nanoTime))) {
        logRequestResponse(LoggingMagnet(log(logLevel, hasRemoteAddress = hasRemoteAddress) _)) {
          handleExceptions(ExceptionHandler.default) {
            handleRejections(RejectionHandler.Default) {
              inner
            }
          }
        }
      }
    }
  }

  private def log(logLevel: LogLevel, hasRemoteAddress: Boolean)(request: HttpRequest): Any ⇒ Unit = response ⇒ {
    val isFailure = PartialFunction.cond(response) {
      case response: HttpResponse ⇒ response.status.isFailure
    }
    webLogger.log(
      if (isFailure) Warn else logLevel,
      requestResponseToLine(request, response, hasRemoteAddress = hasRemoteAddress))
  }

  private def requestResponseToLine(request: HttpRequest, response: Any, hasRemoteAddress: Boolean) = {
    val remoteAddress = (hasRemoteAddress option (request.header[`Remote-Address`] map { _.address })).flatten getOrElse RemoteAddress.Unknown
    val sb = new StringBuilder(500)
    sb.append(remoteAddress.toOption map { _.getHostAddress } getOrElse "-")
    sb.append(" ")
    sb.append(request.method)
    sb.append(' ')
    sb.append(request.uri)
    sb.append(' ')
    sb.append(request.protocol)
    response match {
      case response: HttpResponse ⇒
        sb.append(' ')
        sb.append(response.status.intValue)
        if (response.status.isFailure) {
            sb.append(' ')
          response.entity match {
            case entity @ HttpEntity.NonEmpty(`text/plain(UTF-8)`, _) ⇒  // Error message body is only logged if encoded as "text/plain; charset=UTF-8". Don't rely on this !!!
              sb.append(entity.asString take 200 takeWhile { c ⇒ !c.isControl })
            case _ ⇒
              sb.append(response.status.reason)
          }
        } else {
          sb.append(' ')
          sb.append(response.entity.data.length)
        }
      case _ ⇒
    }
    for (`X-JobScheduler-Request-Started-At`(startedAt) ← request.header[`X-JobScheduler-Request-Started-At`]) {
      sb.append(' ')
      sb.append(Duration.ofNanos(System.nanoTime - startedAt).pretty)
    }
    sb.toString
  }

  private case class `X-JobScheduler-Request-Started-At`(nanos: Long) extends HttpHeader  {
    def name = "X-JobScheduler-Request-Started-At"
    def value = nanos.toString
    val lowercaseName = name.toLowerCase
    def render[R <: Rendering](r: R): r.type = r ~~ name ~~ ':' ~~ ' ' ~~ value
  }
}
