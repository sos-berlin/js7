package js7.controller.web.controller.api.test

import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import js7.base.auth.SimpleUser
import js7.common.akkahttp.ExceptionHandling
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.configutils.Configs._
import js7.common.message.ProblemCodeMessages
import js7.controller.configuration.ControllerConfiguration.DefaultConfig
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
    WebServerBinding.Http,
    GateKeeper.Configuration.fromConfig(
      config"js7.web.server.auth.loopback-is-public = true"
        withFallback DefaultConfig,
      SimpleUser.apply),
    isLoopback = true)

  protected final lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)

  protected def config = config"""
    js7.web.server.verbose-error-messages = on
    js7.web.server.services.event.streaming.delay = 20ms
    js7.web.server.services.event.streaming.chunk-timeout = 1h
    """
}
