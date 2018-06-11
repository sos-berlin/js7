package com.sos.jobscheduler.agent.web.test

import akka.actor.ActorRefFactory
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.web.common.AgentRouteProvider
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.WebLogDirectives
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{LoginSession, SessionRegister}
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser with BeforeAndAfterAll with ScalatestRouteTest {
  this: AgentRouteProvider with Suite ⇒

  protected def uriPathPrefix = ""

  protected final val gateKeeper = new GateKeeper(
    GateKeeper.Configuration.fromConfig(testConfig, SimpleUser.apply),
    TimerService(idleTimeout = Some(1.s)),
    isHttps = false)

  protected final val sessionRegister = SessionRegister.start[LoginSession.Simple](
    actorRefFactory, LoginSession.Simple.apply, akkaAskTimeout = 99.seconds)

  override def testConfig = AgentConfiguration.DefaultsConfig
  protected final def actorSystem = system
  protected val config = WebLogDirectives.TestConfig

  implicit val routeTestTimeout = RouteTestTimeout(5.seconds)

  /** Provide ActorRefFactory for some Routes. */
  protected final def actorRefFactory: ActorRefFactory = system

  override protected def afterAll() = {
    closer.close()
    cleanUp()
    super.afterAll()
  }
}
