package com.sos.scheduler.engine.agent.web.test

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.web.common.AgentWebService
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.sprayutils.web.auth.GateKeeper
import com.sos.scheduler.engine.common.sprayutils.web.session.SessionRegister
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.duration._
import spray.testkit.ScalatestRouteTest

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser with BeforeAndAfterAll with ScalatestRouteTest {
  this: AgentWebService with Suite ⇒

  protected def uriPathPrefix = ""

  protected val sessionRegister = SessionRegister.forTest

  implicit val routeTestTimeout = RouteTestTimeout(5.seconds)

  /** Provide ActorRefFactory for some Routes. */
  protected final def actorRefFactory: ActorRefFactory = system

  override protected def afterAll() = {
    closer.close()
    super.afterAll()
  }

  protected lazy val route = buildRoute(GateKeeper.forTest)
}
