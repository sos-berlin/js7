package com.sos.scheduler.engine.common.sprayutils

import com.sos.scheduler.engine.common.log.LogLevel._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.{addHeader, passIf}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.typesafe.config.Config
import java.time.Duration
import spray.http.ContentTypes.`text/plain(UTF-8)`
import spray.http.HttpHeaders.{`Remote-Address`, `X-Forwarded-For`}
import spray.http.{HttpEntity, HttpHeader, HttpRequest, HttpResponse, RemoteAddress, Rendering}
import spray.routing.Directives._
import spray.routing._
import spray.routing.directives.LoggingMagnet

/**
  * @author Joacim Zschimmer
  */
object WebLogDirectives {
  private val webLogger = Logger("Web")

  def handleErrorAndLog(subConfig: Config)(implicit routingSettings: RoutingSettings): Directive0 = {
    val withTimestamp = subConfig.getBoolean("log.elapsed-time")
    mapInnerRoute { inner ⇒
      (passIf(!withTimestamp) | addHeader(`X-JobScheduler-Request-Started-At`(System.nanoTime))) {
        logRequestResponse(LoggingMagnet(log _)) {
          handleExceptions(ExceptionHandler.default) {
            handleRejections(RejectionHandler.Default) {
              inner
            }
          }
        }
      }
    }
  }

  private def log(request: HttpRequest): Any ⇒ Unit = response ⇒ {
    val isFailure = PartialFunction.cond(response) {
      case response: HttpResponse ⇒ response.status.isFailure
    }
    val logLevel = if (isFailure) Warn else Debug
    webLogger.log(logLevel, requestResponseToLine(request, response))
  }

  private def requestResponseToLine(request: HttpRequest, response: Any) = {
    val remoteAddress = (request.header[`X-Forwarded-For`] flatMap { _.addresses.headOption }
      orElse (request.header[`Remote-Address`] map { _.address })
      orElse (request.headers collectFirst { case h if h.lowercaseName == "x-real-ip" ⇒ RemoteAddress(h.value) })
      getOrElse RemoteAddress.Unknown)
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
