package com.sos.jobscheduler.common.akkahttp

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import akka.http.scaladsl.server.Directives.{complete, extractRequest}
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import com.sos.jobscheduler.base.exceptions.PublicException
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
      case e: HttpStatusCodeException ⇒
        complete((e.statusCode, e.problem))

      case e: PublicException ⇒
        extractRequest { request ⇒
          webLogger.warn(toLogMessage(request, e), e)
          complete((BadRequest, Problem.eager(e)))
        }

      case e ⇒
        extractRequest { request ⇒
          webLogger.warn(toLogMessage(request, e), e)
          if (respondWithException)
            complete((InternalServerError, Problem.eager(e)))
          else
            complete(InternalServerError)
        }
    }

  protected final def seal(route: Route) =
    Route.seal(route)(exceptionHandler = exceptionHandler)
}

object ExceptionHandling
{
  private val webLogger = Logger("web.exception")

  private def toLogMessage(request: HttpRequest, throwable: Throwable) =
    s"Error while handling ${request.method.value} ${request.uri}: ${throwable.toStringWithCauses}"
}
