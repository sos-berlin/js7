package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.web.AgentExceptionHandler._
import com.sos.scheduler.engine.base.exceptions.PublicException
import com.sos.scheduler.engine.common.scalautil.Logger
import spray.http.HttpRequest
import spray.http.StatusCodes._
import spray.routing.Directives._
import spray.routing.ExceptionHandler
import spray.util.LoggingContext

/**
  * @author Joacim Zschimmer
  */
trait AgentExceptionHandler {

  implicit protected def exceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e: PublicException =>
        requestInstance { request ⇒
          logger.debug(toLogMessage(request, e), e)
          complete(BadRequest, e.publicMessage)
        }
      case e: Exception ⇒
        requestInstance { request ⇒
          logger.warn(toLogMessage(request, e), e)
          complete(InternalServerError, e.getClass.getSimpleName + (Option(e.getMessage) map { o ⇒ s": $o" } getOrElse ""))  // We expose the message of every error !!!
        }
    }
}

object AgentExceptionHandler {
  private val logger = Logger(getClass)

  private def toLogMessage(request: HttpRequest, throwable: Throwable) =
    s"Error while handling ${request.method} ${request.uri}: $throwable"
}
