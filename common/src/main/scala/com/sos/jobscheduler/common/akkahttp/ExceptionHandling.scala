package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.StatusCodes.{InternalServerError, ServiceUnavailable}
import akka.http.scaladsl.model.{HttpRequest, StatusCode}
import akka.http.scaladsl.server.Directives.{complete, extractRequest}
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.akkahttp.ExceptionHandling._
import com.sos.jobscheduler.common.akkahttp.StandardMarshallers._
import com.sos.jobscheduler.common.scalautil.Logger
import com.typesafe.config.Config

/**
  * @author Joacim Zschimmer
  */
trait ExceptionHandling
{
  protected def config: Config

  private lazy val respondWithException = config.getBoolean("jobscheduler.webserver.verbose-error-messages")

  implicit protected val exceptionHandler: ExceptionHandler =
    ExceptionHandler {
      case e: HttpStatusCodeException =>
        complete(e.statusCode -> e.problem)

      case e: akka.pattern.AskTimeoutException =>
        if (isShuttingDown) {
          extractRequest { request =>
            webLogger.debug(toLogMessage(request, e), e)
            complete(ServiceUnavailable -> Problem.pure("Shutting down"))
          }
        } else
          completeWithError(ServiceUnavailable, e)

      case e =>
        completeWithError(InternalServerError, e)
    }

  private def completeWithError(status: StatusCode, e: Throwable): Route =
    extractRequest { request =>
      def msg = toLogMessage(request, e)
      if (isShuttingDown) webLogger.debug(msg, e) else webLogger.warn(msg, e)
      if (respondWithException)
        complete(status -> Problem.pure(e))
      else
        complete(status)
    }

  protected final def seal(route: Route) =
    Route.seal(route)(exceptionHandler = exceptionHandler)
}

object ExceptionHandling
{
  val webLogger = Logger("jobscheduler.web.exception")

  private def toLogMessage(request: HttpRequest, throwable: Throwable) =
    s"Error while handling ${request.method.value} ${request.uri}: ${throwable.toStringWithCauses}"
}
