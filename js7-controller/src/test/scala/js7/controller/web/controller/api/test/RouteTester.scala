package js7.controller.web.controller.api.test

import cats.effect.unsafe.IORuntime
import js7.base.auth.SimpleUser
import js7.base.configutils.Configs.*
import js7.base.time.ScalaTime.*
import js7.common.message.ProblemCodeMessages
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.{SessionRegister, SimpleSession}
import js7.common.pekkohttp.{ExceptionHandling, PekkoHttpUtils}
import js7.controller.configuration.ControllerConfiguration.DefaultConfig
import org.apache.pekko.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import org.scalatest.Suite
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
trait RouteTester extends ScalatestRouteTest, ExceptionHandling:
  this: Suite =>

  ProblemCodeMessages.initialize()

  /** For RouteTest responseAs[]. */
  private implicit val routeTestDuration: Duration = 9.s
  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(9.s)

  private given IORuntime = ioRuntime

  protected final lazy val gateKeeper = new GateKeeper(
    WebServerBinding.localhostHttp(port = 1),
    GateKeeper.Configuration.fromConfig(
      config"js7.web.server.auth.loopback-is-public = true"
        .withFallback(DefaultConfig),
      SimpleUser.apply),
    isLoopback = true)

  protected final lazy val sessionRegister =
    SessionRegister.forTest(SimpleSession.apply, SessionRegister.TestConfig)

  override final def testConfig = config

  protected def config = config"""
    pekko.http.host-connection-pool.response-entity-subscription-timeout = 10s
    pekko.loggers = ["org.apache.pekko.event.slf4j.Slf4jLogger"]
    pekko.stdout-loglevel = "ERROR"
    #pekko.loglevel = warning
    js7.web.client.compression = off
    js7.web.server.verbose-error-messages = on
    js7.web.server.services.event.streaming.delay = 20ms
    js7.web.server.services.event.streaming.timeout = 1h
    js7.web.server.services.streaming-post-size-limit-factor = 50%
    js7.web.server.services.command-size-limit = 8m
    """

  override def beforeAll() =
    PekkoHttpUtils.avoidLazyObjectInitializationDeadlock()
    super.beforeAll()

  override def afterAll() =
    try
      cleanUp()
    finally
      super.afterAll()
