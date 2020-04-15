package com.sos.jobscheduler.master.web.master.api.test

import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.akkahttp.ExceptionHandling
import com.sos.jobscheduler.common.akkahttp.web.auth.GateKeeper
import com.sos.jobscheduler.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.master.configuration.MasterConfiguration.DefaultConfig
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Suite
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait RouteTester extends ScalatestRouteTest with ExceptionHandling
{
  this: Suite =>

  ProblemCodeMessages.initialize()

  /** For RouteTest responseAs[]. */
  private implicit val routeTestDuration: Duration = 9.seconds
  private implicit val routeTestTimeout = RouteTestTimeout(9.seconds)

  protected final lazy val gateKeeper = new GateKeeper(
    GateKeeper.Configuration.fromConfig(
      ConfigFactory.parseString("jobscheduler.webserver.auth.loopback-is-public = true")
        withFallback DefaultConfig,
      SimpleUser.apply),
    isLoopback = true)

  protected final lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)

  protected def config = ConfigFactory.parseString(
    """jobscheduler.webserver.verbose-error-messages = on
      |jobscheduler.webserver.services.event.streaming.delay = 20ms
      |jobscheduler.webserver.services.event.streaming.chunk-timeout = 1h
      |""".stripMargin)
}
