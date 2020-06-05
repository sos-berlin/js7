package js7.master.web.master.api.test

import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import js7.base.auth.SimpleUser
import js7.common.akkahttp.ExceptionHandling
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.core.message.ProblemCodeMessages
import js7.master.configuration.MasterConfiguration.DefaultConfig
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
