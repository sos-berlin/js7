package com.sos.jobscheduler.master.web.master.api.test

import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.ExceptionHandling
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.master.configuration.MasterConfiguration.DefaultConfig
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Suite
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait RouteTester extends ScalatestRouteTest with ExceptionHandling {
  this: Suite =>

  private implicit val routeTestTimeout = RouteTestTimeout(10.seconds)

  protected final lazy val gateKeeper = new GateKeeper(
    GateKeeper.Configuration.fromConfig(
      ConfigFactory.parseString("jobscheduler.webserver.auth.loopback-is-public = true")
        withFallback DefaultConfig,
      SimpleUser.apply),
    isLoopback = true)

  protected final lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)

  protected val config = ConfigFactory.parseString(
    """jobscheduler.webserver.verbose-error-messages = on
      |jobscheduler.webserver.event.streaming.delay = 20ms
      |jobscheduler.webserver.event.streaming.chunk-timeout = 1h
      |""".stripMargin)
}
