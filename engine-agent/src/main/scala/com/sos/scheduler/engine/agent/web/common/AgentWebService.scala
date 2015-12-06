package com.sos.scheduler.engine.agent.web.common

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.web.common.AgentWebService._
import com.sos.scheduler.engine.common.scalautil.Logger
import scala.collection.mutable
import spray.http.Uri.Path
import spray.routing.Directives._
import spray.routing.Route

/**
 * Standard trait for Agent web services.
 * To be able to mix-in multiple web services, use `addRoute` to add a `Route`.
 * Method `route` returns the combined `Route`.
 *
 * @author Joacim Zschimmer
 */
trait AgentWebService {
  protected implicit def actorRefFactory: ActorRefFactory
  /**
   * URI path prefix without prefix or suffix slashes.
   */
  protected def uriPathPrefix: String = ""
  protected final lazy val jobschedulerPath = Path(List(uriPathPrefix, "jobscheduler") filter { _.nonEmpty } mkString "/")
  protected final lazy val agentPath = Path(s"$jobschedulerPath/$AgentPrefix")

  private lazy val jobschedulerStandard =
    decompressRequest() & compressResponseIfRequested(()) & pathPrefix(separateOnSlashes(jobschedulerPath.toString))

  private val addedRoutes = mutable.Buffer[Entry]()

  protected def addApiRoute(route: ⇒ Route): Unit =
    addJobschedulerRoute {
      pathPrefix(AgentPrefix / "api") {
        route
      }
    }

  /**
   * All added routes are combined by method `route`.
   */
  protected def addJobschedulerRoute(route: ⇒ Route): Unit = add(jobschedulerStandard { route })

  protected def addRawRoute(rawRoute: ⇒ Route): Unit = add(rawRoute)

  private def add(rawRoute: ⇒ Route) = addedRoutes += Entry(() ⇒ rawRoute, callerMethodString)

  /**
   * Returns a route, consisting of all added routes.
   */
  final def route: Route = {
    for (o ← addedRoutes map { _.callerName }) logger.trace(s"Using route of $o")
    val routes = addedRoutes map { _.routeFactory() }
    routes reduce { _ ~ _ }
  }
}

object AgentWebService {
  val AgentPrefix = "agent"
  private val logger = Logger(getClass)
  private val PackageName = getClass.getPackage.getName

  private def callerMethodString: String =
    (new Exception).getStackTrace.toIterator map { _.toString } find { o ⇒ !(o contains PackageName) } getOrElse "?"

  private case class Entry(routeFactory: () ⇒ Route, callerName: String)
}
