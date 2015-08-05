package com.sos.scheduler.engine.agent.web.common

import akka.actor.ActorRefFactory
import com.google.common.base.Strings.isNullOrEmpty
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
  protected implicit def actorRefFactory: ActorRefFactory
  /**
   * URI path prefix without prefix or suffix slashes.
   */
  protected def uriPrefix: String = ""
  protected final lazy val jobschedulerPath = List(uriPrefix, "jobscheduler") filter { _.nonEmpty } mkString "/"

  private lazy val jobschedulerStandard =
    decompressRequest() & compressResponseIfRequested(()) & pathPrefix(separateOnSlashes(jobschedulerPath))

  private val addedRoutes = mutable.Buffer[Entry]()

  implicit def exceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e ⇒
        requestUri { uri ⇒
          logger.debug(s"Request $uri: $e", e)
          val msg = e match {
            case e: RuntimeException if !isNullOrEmpty(e.getMessage) ⇒ e.getMessage
            case _ ⇒ e.toString
          }
          complete(InternalServerError, msg)
        }
    }

  /**
   * All added routes are combined by method `route`.
   */
  protected def addRoute(route: ⇒ Route): Unit = add(jobschedulerStandard { route })

  protected def addRawRoute(rawRoute: ⇒ Route): Unit = add(rawRoute)

  private def add(rawRoute: ⇒ Route) = {
    val callerString = (new Exception).getStackTrace()(3).toString  // Same nesting depth for addRoute and addRawRoute
    addedRoutes += Entry(() ⇒ rawRoute, callerString)
  }

  /**
   * Returns a route, consisting of all added routes.
   */
  final def route: Route = {
    for (o ← addedRoutes map { _.callerName }) logger.trace(s"Using route of $o")
    val routes = addedRoutes map { _.routeFactory() }
    routes reduce { _ ~ _ }
  }
}

object ServiceStandards {
  private val logger = Logger(getClass)

  private case class Entry(routeFactory: () ⇒ Route, callerName: String)
}
