package com.sos.scheduler.engine.agent.web.common

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.web.common.ServiceStandards._
import com.sos.scheduler.engine.common.scalautil.Logger
import scala.collection.mutable
import spray.http.StatusCodes.InternalServerError
import spray.routing.Directives._
import spray.routing.{ExceptionHandler, Route}
import spray.util.LoggingContext

/**
 * Standard trait for Agent web services.
 * To be able to mix-in multiple web services, use `addRoute` to add a `Route`.
 * Method `route` returns the combined `Route`.
 *
 * @author Joacim Zschimmer
 */
trait ServiceStandards {
  implicit def actorRefFactory: ActorRefFactory

  private val agentStandard = decompressRequest() & compressResponseIfRequested(()) & pathPrefix("jobscheduler")
  private val routeBuffer = mutable.Buffer[(String, Route)]()

  implicit def exceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e ⇒
        requestUri { uri ⇒
          logger.debug(s"Request $uri: $e", e)
          complete(InternalServerError, e.getMessage stripPrefix "java.lang.RuntimeException: ")
        }
    }

  /**
   * All added routes are combined by method `route`.
   */
  protected def addRoute(route: ⇒ Route): Unit = addRawRoute(agentStandard { route })

  protected def addRawRoute(rawRoute: ⇒ Route): Unit = {
    val callerString = (new Exception).getStackTrace()(2).toString
    routeBuffer += callerString → rawRoute
  }

  /**
   * Returns a route, consisting of all added routes.
   */
  final def route: Route = {
    for (o ← routeBuffer map { _._1 }) logger.trace(s"Using route of $o")
    routeBuffer map { _._2 } reduce { _ ~ _ }
  }
}

object ServiceStandards {
  private val logger = Logger(getClass)
}
