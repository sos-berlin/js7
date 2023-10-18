package js7.agent.web.test

import org.apache.pekko.actor.ActorRefFactory
import org.apache.pekko.http.scaladsl.model.HttpHeader
import org.apache.pekko.http.scaladsl.model.headers.RawHeader
import org.apache.pekko.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import js7.agent.configuration.AgentConfiguration
import js7.agent.web.common.AgentRouteProvider
import js7.base.Js7Version
import js7.base.auth.{HashedPassword, SimpleUser, UserId}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.HasCloser
import js7.common.pekkohttp.WebLogDirectives
import js7.common.pekkohttp.web.auth.GateKeeper
import js7.common.pekkohttp.web.data.WebServerBinding
import js7.common.pekkohttp.web.session.SessionRegister
import js7.common.http.PekkoHttpClient.`x-js7-session`
import js7.common.message.ProblemCodeMessages
import js7.subagent.SubagentSession
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.{BeforeAndAfterAll, Suite}

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser with BeforeAndAfterAll with ScalatestRouteTest:
  this: AgentRouteProvider & Suite =>

  ProblemCodeMessages.initialize()

  protected def uriPathPrefix = ""

  protected final val gateKeeper = new GateKeeper(
    WebServerBinding.Http,
    GateKeeper.Configuration.fromConfig(testConfig, SimpleUser.apply))

  protected final val sessionRegister = SessionRegister.forTest[SubagentSession](
    actorRefFactory, SubagentSession.apply, SessionRegister.TestConfig)

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
      .await(99.s).orThrow
    RawHeader(`x-js7-session`.name, token.secret.string)

  /** Provide ActorRefFactory for some Routes. */
  protected final def actorRefFactory: ActorRefFactory = system

  override protected def afterAll() =
    closer.close()
    cleanUp()
    super.afterAll()
