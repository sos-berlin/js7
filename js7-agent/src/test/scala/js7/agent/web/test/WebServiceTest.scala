package js7.agent.web.test

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import js7.agent.configuration.AgentConfiguration
import js7.agent.web.common.AgentRouteProvider
import js7.base.auth.{HashedPassword, SessionToken, SimpleUser, UserId}
import js7.base.utils.HasCloser
import js7.common.akkahttp.WebLogDirectives
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.data.WebServerBinding
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.message.ProblemCodeMessages
import js7.common.scalautil.MonixUtils.syntax._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait WebServiceTest extends HasCloser with BeforeAndAfterAll with ScalatestRouteTest {
  this: AgentRouteProvider with Suite =>

  ProblemCodeMessages.initialize()

  protected def uriPathPrefix = ""

  protected final val gateKeeper = new GateKeeper(
    WebServerBinding.Http,
    GateKeeper.Configuration.fromConfig(testConfig, SimpleUser.apply))

  protected final val sessionRegister = SessionRegister.start[SimpleSession](
    actorRefFactory, SimpleSession.apply, SessionRegister.TestConfig)

  override def testConfig = AgentConfiguration.DefaultConfig
  protected final def actorSystem = system
  protected val config = WebLogDirectives.TestConfig

  implicit val routeTestTimeout = RouteTestTimeout(5.seconds)

  protected lazy val testSessionHeader: HttpHeader = {
    val token = sessionRegister.login(SimpleUser(UserId("SOME-USER"), HashedPassword.MatchesNothing), None)
      .await(99.seconds)
    RawHeader(SessionToken.HeaderName, token.secret.string)
  }

  /** Provide ActorRefFactory for some Routes. */
  protected final def actorRefFactory: ActorRefFactory = system

  override protected def afterAll() = {
    closer.close()
    cleanUp()
    super.afterAll()
  }
}
