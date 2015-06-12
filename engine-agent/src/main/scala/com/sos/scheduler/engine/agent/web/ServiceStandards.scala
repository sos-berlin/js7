package com.sos.scheduler.engine.agent.web

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.web.ServiceStandards._
import com.sos.scheduler.engine.common.scalautil.Logger
import spray.http.StatusCodes.InternalServerError
import spray.routing.Directives._
import spray.routing.ExceptionHandler
import spray.util.LoggingContext

/**
 * @author Joacim Zschimmer
 */
trait ServiceStandards {
  implicit def actorRefFactory: ActorRefFactory

  protected final val agentStandard = decompressRequest() & compressResponseIfRequested(()) & pathPrefix("jobscheduler")

  implicit def exceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e ⇒
        requestUri { uri ⇒
          logger.debug(s"Request $uri: $e", e)
          complete(InternalServerError, e.getMessage stripPrefix "java.lang.RuntimeException: ")
        }
    }
}

object ServiceStandards {
  private val logger = Logger(getClass)
}
