package com.sos.scheduler.engine.agent.web.common

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.web.common.AgentWebService._
import com.sos.scheduler.engine.common.auth.Account
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichSeq
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import scala.collection.mutable
import scala.concurrent.blocking
import spray.http.Uri.Path
import spray.routing.AuthenticationFailedRejection.CredentialsRejected
import spray.routing.Directives._
import spray.routing.authentication._
import spray.routing.{AuthenticationFailedRejection, RejectionHandler, Route}

/**
 * Standard trait for Agent web services.
 * To be able to mix-in multiple web services, use `addRoute` to add a `Route`.
 * Method `route` returns the combined `Route`.
 *
 * @author Joacim Zschimmer
 */
trait AgentWebService extends AgentExceptionHandler {
  protected implicit def actorRefFactory: ActorRefFactory
  protected def authenticator: UserPassAuthenticator[Account]

  implicit private def executionContext = actorRefFactory.dispatcher

  /**
   * URI path prefix without prefix or suffix slashes.
   */
  protected def uriPathPrefix: String = ""
  protected final lazy val jobschedulerPath = Path(List(uriPathPrefix, "jobscheduler") filter { _.nonEmpty } mkString "/")
  protected final lazy val agentPath = Path(s"$jobschedulerPath/$AgentPrefix")

  private val rawRouteEntries = mutable.Buffer[Entry]()
  private val jobschedulerStandardRouteEntries = mutable.Buffer[Entry]()
  private val apiRouteEntries = mutable.Buffer[Entry]()
  private val unauthenticatedApiRouteEntries = mutable.Buffer[Entry]()

  private def allEntries = rawRouteEntries ++ jobschedulerStandardRouteEntries ++ apiRouteEntries

  protected def addApiRoute(route: ⇒ Route): Unit =
    apiRouteEntries += Entry.of(route, "addApiRoute")

  protected def addUnauthenticatedApiRoute(route: ⇒ Route): Unit =
    unauthenticatedApiRouteEntries += Entry.of(route, "addUnauthenticatedApiRoute")

  /**
   * All added routes are combined by method `route`.
   */
  protected def addJobschedulerRoute(route: ⇒ Route): Unit =
    jobschedulerStandardRouteEntries += Entry.of(route, "addJobschedulerRoute")

  protected def addRawRoute(rawRoute: ⇒ Route): Unit =
    rawRouteEntries += Entry.of(rawRoute, "addRawRoute")

  /**
   * Returns a route, consisting of all added routes.
   */
  final def route: Route = {
    for (e ← allEntries) logger.trace(s"Using route of ${e.callerName}")
    (decompressRequest() & compressResponseIfRequested(())) {
      pathPrefix(separateOnSlashes(jobschedulerPath.toString)) {
        pathPrefix(AgentPrefix / "api") {
          handleRejections(failIfCredentialsRejected) {
            authenticate(BasicAuth(authenticator, realm = Realm)) { _ ⇒
              toRoute(apiRouteEntries)
            }
          } ~
            toRoute(unauthenticatedApiRouteEntries)
        } ~
          toRoute(jobschedulerStandardRouteEntries)
      }
    } ~
      toRoute(rawRouteEntries)
  }

  private def failIfCredentialsRejected = RejectionHandler {
    case rejections @ AuthenticationFailedRejection(CredentialsRejected, headers) :: _ ⇒
      detach(()) {
        logger.warn(s"HTTP request with invalid authentication rejected")
        blocking {
          sleep(InvalidAuthenticationDelay)
        }
        RejectionHandler.Default(rejections)
      }
  }
}

object AgentWebService {
  val AgentPrefix = "agent"
  val InvalidAuthenticationDelay = 1.s
  private val logger = Logger(getClass)
  private val PackageName = getClass.getPackage.getName
  private val Realm = "JobScheduler Agent"

  private def callerMethodString(methodName: String): String =
    new Exception().getStackTrace.toIterator.map { _.toString }
      .dropWhile { o ⇒ (o contains PackageName) || (o contains s".$methodName(") }
      .find { _ ⇒ true }   // Like headOption
      .getOrElse("?")

  private case class Entry(route: () ⇒ Route, callerName: String)

  private object Entry {
    def of(rawRoute: ⇒ Route, methodName: String) = new Entry(() ⇒ rawRoute, callerMethodString(methodName))
  }

  private def toRoute(entries: Seq[Entry]): Route = entries.map(_.route()).foldFast(reject)(_ ~ _)
}
