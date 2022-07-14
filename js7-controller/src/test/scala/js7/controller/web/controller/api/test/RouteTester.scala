package js7.controller.web.controller.api.test

import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.*
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.time.ScalaTime.*
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.akkahttp.{AkkaHttpUtils, ExceptionHandling}
import js7.common.message.ProblemCodeMessages
import js7.controller.configuration.ControllerConfiguration.DefaultConfig
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.Suite
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
trait RouteTester extends ScalatestRouteTest with ExceptionHandling
{
  this: Suite =>

  coupleScribeWithSlf4j()
  ProblemCodeMessages.initialize()

  /** For RouteTest responseAs[]. */
  private implicit val routeTestDuration: Duration = 9.s
  private implicit val routeTestTimeout = RouteTestTimeout(9.s)

  protected final lazy val gateKeeper = new GateKeeper(
    WebServerBinding.Http,
    GateKeeper.Configuration.fromConfig(
      config"js7.web.server.auth.loopback-is-public = true"
        withFallback DefaultConfig,
      SimpleUser.apply),
    isLoopback = true)

  protected final lazy val sessionRegister =
    SessionRegister.start[SimpleSession](system, SimpleSession.apply, SessionRegister.TestConfig)

  override final def testConfig = config

  protected def config = config"""
    akka.http.host-connection-pool.response-entity-subscription-timeout = 10s
    js7.web.client.compression = off
    js7.web.server.verbose-error-messages = on
    js7.web.server.services.event.streaming.delay = 20ms
    js7.web.server.services.event.streaming.chunk-timeout = 1h
    js7.web.server.services.streaming-post-size-limit-factor = 50%
    js7.web.server.services.command-size-limit = 8m
    """

  override def beforeAll() = {
    AkkaHttpUtils.avoidLazyObjectInitializationDeadlock()
    super.beforeAll()
  }

  override def afterAll() = {
    cleanUp()
    super.afterAll()
  }
}
