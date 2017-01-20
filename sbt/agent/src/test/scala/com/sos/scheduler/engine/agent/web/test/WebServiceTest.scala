package com.sos.scheduler.engine.agent.web.test

import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
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

  protected implicit final lazy val actorRefFactory = newActorSystem(getClass.getSimpleName)(closer)

  override protected def afterAll() = closer.close()

  protected lazy val route = buildRoute(GateKeeper.forTest)
}
