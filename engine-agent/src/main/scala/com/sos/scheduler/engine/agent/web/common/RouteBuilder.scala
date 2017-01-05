package com.sos.scheduler.engine.agent.web.common

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.web.common.RouteBuilder._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichSeq
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.SprayUtils.pathSegments
import com.sos.scheduler.engine.common.sprayutils.web.auth.GateKeeper
import com.sos.scheduler.engine.common.sprayutils.web.session.SessionRegister
import scala.collection.mutable
import spray.routing.Directives._
import spray.routing.Route

/**
  * @author Joacim Zschimmer
  */
final class RouteBuilder(sessionRegister: SessionRegister[Unit]) extends Mutable {
  private val otherRoutes = mutable.Buffer[CallersRoute]()
  private val prefixedRoutes = mutable.Buffer[CallersRoute]()
  private val jobschedulerStandardRoutes = mutable.Buffer[CallersRoute]()
  private val apiRoutes = mutable.Buffer[CallersRoute]()
  private val unauthenticatedApiRoutes = mutable.Buffer[CallersRoute]()

  def addRootRoute(route: ⇒ Route): Unit =
    otherRoutes += CallersRoute.of("addRootRoute", route)

  /**
    * Routes prefixed with uriPathPrefix.
    */
  def addPrefixedRoute(route: ⇒ Route): Unit =
    prefixedRoutes += CallersRoute.of("addPrefixedRoute", route)

  /**
    * Routes for non-authenticated /jobscheduler.
    */
  def addJobschedulerRoute(route: ⇒ Route): Unit =
    jobschedulerStandardRoutes += CallersRoute.of("addJobschedulerRoute", route)

  /**
    * Routes for authenticated /jobscheduler/agent/api.
    */
  def addApiRoute(route: ⇒ Route): Unit =
    apiRoutes += CallersRoute.of("addApiRoute", route)

  /**
    * Routes for non-authenticated /jobscheduler/agent/api.
    */
  def addUnauthenticatedApiRoute(route: ⇒ Route): Unit =
    unauthenticatedApiRoutes += CallersRoute.of("addUnauthenticatedApiRoute", route)

  private def logAllEntries(): Unit = {
    val all = otherRoutes ++ prefixedRoutes ++ jobschedulerStandardRoutes ++ apiRoutes ++ unauthenticatedApiRoutes
    for (e ← all) logger.trace(s"Using route of ${e.callerName}")
  }

  def ++=(o: RouteBuilder): Unit = {
    otherRoutes ++= o.otherRoutes
    prefixedRoutes ++= o.prefixedRoutes
    jobschedulerStandardRoutes ++= o.jobschedulerStandardRoutes
    apiRoutes ++= o.apiRoutes
    unauthenticatedApiRoutes ++= o.unauthenticatedApiRoutes
  }

  def buildRoute(gateKeeper: GateKeeper, uriPathPrefix: String)
    (implicit actorRefFactory: ActorRefFactory): Route =
  {
    implicit val executionContext = actorRefFactory.dispatcher

    val apiRoute =
      sessionRegister.directives.session { _ ⇒
        gateKeeper.retrictRelaxed {
          toRoute(apiRoutes)
        }
      } ~
      gateKeeper.restrict.apply {
        toRoute(apiRoutes)
      } ~
      gateKeeper.retrictRelaxed {
        toRoute(unauthenticatedApiRoutes)
      }

    logAllEntries()

    (decompressRequest() & compressResponseIfRequested(())) {
      possiblyEmptyPathPrefix(uriPathPrefix) {
        pathSegments("jobscheduler") {
          pathSegments("agent/api") {
            apiRoute
          } ~
            toRoute(jobschedulerStandardRoutes)
        } ~
          toRoute(prefixedRoutes)
      } ~
        toRoute(otherRoutes)
    }
  }
}

object RouteBuilder {
  private val logger = Logger(getClass)
  private val PackageName = getClass.getPackage.getName

  private def possiblyEmptyPathPrefix(uriPathPrefix: String) = if (uriPathPrefix.isEmpty) pass else pathPrefix(separateOnSlashes(uriPathPrefix))

  private def callerMethodString(methodName: String): String =
    new Exception().getStackTrace.toIterator.map { _.toString }
      .dropWhile { o ⇒ (o contains PackageName) || (o contains s".$methodName(") }
      .find { _ ⇒ true }   // Like headOption
      .getOrElse("?")

  private case class CallersRoute(callerName: String, route: () ⇒ Route)

  private object CallersRoute {
    def of(methodName: String, rawRoute: ⇒ Route) = new CallersRoute(callerMethodString(methodName), () ⇒ rawRoute)
  }

  private def toRoute(entries: Seq[CallersRoute]): Route = entries.map(_.route()).foldFast(reject)(_ ~ _)
}
