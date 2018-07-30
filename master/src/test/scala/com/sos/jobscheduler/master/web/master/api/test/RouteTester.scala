package com.sos.jobscheduler.master.web.master.api.test

import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.master.configuration.MasterConfiguration.DefaultConfig
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler
import org.scalatest.Suite
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait RouteTester extends ScalatestRouteTest {
  this: Suite ⇒

  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)

  protected final lazy val gateKeeper = new GateKeeper(
    GateKeeper.Configuration.fromConfig(
      ConfigFactory.parseString("jobscheduler.webserver.auth.loopback-is-public = true")
        withFallback DefaultConfig,
      SimpleUser.apply),
    new TimerService(Some(1.s)),
    isLoopback = true)

  protected final lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)(Scheduler.global)
}
