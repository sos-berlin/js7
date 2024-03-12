package js7.agent.web.test

import cats.effect.unsafe.IORuntime
import org.apache.pekko.actor.ActorRefFactory
import org.apache.pekko.http.scaladsl.model.HttpHeader
import org.apache.pekko.http.scaladsl.model.headers.RawHeader
import org.apache.pekko.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import js7.agent.configuration.AgentConfiguration
import js7.agent.web.common.AgentRouteProvider
import js7.base.Js7Version
import js7.base.auth.{HashedPassword, SimpleUser, UserId}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.HasCloser
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.SessionRegister
import js7.common.http.PekkoHttpClient.`x-js7-session`
import js7.common.message.ProblemCodeMessages
import js7.subagent.SubagentSession
import org.scalatest.{BeforeAndAfterAll, Suite}

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser, BeforeAndAfterAll, ScalatestRouteTest:
  this: AgentRouteProvider & Suite =>

  ProblemCodeMessages.initialize()

  private given IORuntime = ioRuntime

  protected def uriPathPrefix = ""

  protected final lazy val gateKeeper = new GateKeeper(
    WebServerBinding.Http,
    GateKeeper.Configuration.fromConfig(testConfig, SimpleUser.apply))

  protected final lazy val sessionRegister = SessionRegister.forTest[SubagentSession](
    SubagentSession.apply, SessionRegister.TestConfig)

  override def testConfig =
    config"pekko.loglevel = warning"
      .withFallback(AgentConfiguration.DefaultConfig.resolve())
      .withFallback(super.testConfig)

  protected final def actorSystem = system
  protected val config = config"js7.web.server.services.streaming-post-size-limit-factor = 50%"
    .withFallback(WebLogDirectives.TestConfig)

  implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(5.s)

  protected lazy val testSessionHeader: HttpHeader =
    val token = sessionRegister
      .login(SimpleUser(UserId("SOME-USER"), HashedPassword.MatchesNothing), Some(Js7Version))
      .await(99.s)
    RawHeader(`x-js7-session`.name, token.secret.string)

  /** Provide ActorRefFactory for some Routes. */
  protected final def actorRefFactory: ActorRefFactory = system

  override protected def afterAll() =
    closer.close()
    cleanUp()
    super.afterAll()
