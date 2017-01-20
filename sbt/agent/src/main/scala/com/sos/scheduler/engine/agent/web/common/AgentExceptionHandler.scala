package com.sos.scheduler.engine.agent.web.common

import com.google.common.base.Strings.isNullOrEmpty
import com.sos.scheduler.engine.agent.web.common.AgentExceptionHandler._
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

  implicit def exceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e: PublicException =>
        requestInstance { request ⇒
          logger.debug(toLogMessage(request, e), e)
          complete((BadRequest, e.publicMessage))
        }
      case e ⇒
        requestInstance { request ⇒
          logger.debug(toLogMessage(request, e), e)
          val msg = if (e.getClass == classOf[RuntimeException] && !isNullOrEmpty(e.getMessage))
            e.getMessage
          else
            e.toString
          complete((InternalServerError, msg))
        }
    }
//  implicit protected def exceptionHandler(implicit log: LoggingContext) =
//    ExceptionHandler {
//      case e: PublicException =>
//        requestInstance { request ⇒
//          logger.debug(toLogMessage(request, e), e)
//          complete(BadRequest, e.publicMessage)
//        }
//      case e: Exception ⇒
//        requestInstance { request ⇒
//          logger.warn(toLogMessage(request, e), e)
//          val msg = if (e.getClass == classOf[RuntimeException] && !isNullOrEmpty(e.getMessage))
//            e.getMessage
//          else
//            e.toString
//          complete(InternalServerError, msg)  // We expose the message of every error !!!
//        }
//    }
}

object AgentExceptionHandler {
  private val logger = Logger(getClass)

  private def toLogMessage(request: HttpRequest, throwable: Throwable) =
    s"Error while handling ${request.method} ${request.uri}: $throwable"
}
