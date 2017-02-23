package com.sos.jobscheduler.agent.web.test

import akka.actor.ActorRefFactory
import com.sos.jobscheduler.agent.web.common.{AgentWebService, LoginSession}
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.sprayutils.WebLogDirectives
import com.sos.jobscheduler.common.sprayutils.web.auth.GateKeeper
import com.sos.jobscheduler.common.sprayutils.web.session.SessionRegister
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.duration._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser with BeforeAndAfterAll with ScalatestRouteTest {
  this: AgentWebService with Suite ⇒

  protected def uriPathPrefix = ""

  protected val sessionRegister = new SessionRegister[LoginSession]
  protected val actorSystem = system
  protected val config = WebLogDirectives.TestConfig

  implicit val routeTestTimeout = RouteTestTimeout(5.seconds)

  /** Provide ActorRefFactory for some Routes. */
  protected final def actorRefFactory: ActorRefFactory = system

  override protected def afterAll() = {
    closer.close()
    super.afterAll()
  }

  protected lazy val route = buildRoute(GateKeeper.forTest)
}
