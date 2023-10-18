package js7.agent.tests

import js7.agent.RunningAgent
import js7.agent.client.PekkoHttpAgentTextApi
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.tests.PekkoHttpAgentTextApiTest.*
import js7.agent.tests.TestAgentDirectoryProvider.TestUserAndPassword
import js7.base.auth.{HashedPassword, SimpleUser, UserAndPassword}
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceString}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.HasCloser
import js7.common.http.PekkoHttpClient
import js7.common.pekkohttp.web.auth.OurMemoizingAuthenticator
import js7.common.utils.FreeTcpPortFinder.{findFreeLocalUri, findFreeTcpPort}
import monix.execution.Scheduler.Implicits.traced
import org.apache.pekko
import org.apache.pekko.http.scaladsl.model.StatusCodes.*
import org.scalatest.Assertions.*
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class PekkoHttpAgentTextApiTest
extends OurTestSuite, BeforeAndAfterAll, HasCloser, TestAgentProvider:
  
  override protected lazy val agentConfiguration = AgentConfiguration.forTest(
    configAndData = agentDirectory,
    name = "PekkoHttpAgentTextApiTest",
    config"js7.web.server.auth.https-client-authentication = off",   // TODO Test with client certificate
    httpPort = None, httpsPort = Some(findFreeTcpPort()))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    //commandHandler = Some(new CommandHandler {
    //  def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    //  Task {
    //    (command match {
    //      case ExpectedTerminate => Right(AgentCommand.Response.Accepted)
    //      case _ => fail()
    //    }).map(_.asInstanceOf[command.Response])
    //  }
    //}),
    authenticator = Some(_ =>
      new OurMemoizingAuthenticator({
        case TestUserId => Some(SimpleUser(TestUserId, HashedPassword(Password, identity)))
        case _ => None
      })))

  override def beforeAll() =
    provideHttpsFiles()
    super.beforeAll()

  override def afterAll() = closer closeThen { super.afterAll() }

  "Unauthorized when credentials are missing" in:
    autoClosing(newTextAgentClient(None)(_ => ())) { client =>
      interceptUnauthorized:
        client.executeCommand("""{ "TYPE": "ShutDown", "processSignal": "SIGTERM" }""")
    }

  "Unauthorized when credentials are wrong" in:
    autoClosing(newTextAgentClient(Some(TestUserId -> SecretString("WRONG-PASSWORD")))(_ => ())) { client =>
      val e = intercept[PekkoHttpClient.HttpException]:
        client.login() await 99.s
      assert(e.status == Unauthorized)
      assert(e.dataAsString contains "Login: unknown user or invalid password")
    }

  "AgentCommand" in:
    val output = mutable.Buffer.empty[String]
    autoClosing(newTextAgentClient(Some(TestUserId -> Password))(output += _)) { client =>
      client.login() await 99.s
      client.executeCommand("""{ "TYPE": "NoOperation" }""")
      client.getApi("")
    }
    assert(output.size == 2)
    assert(output(0).parseJsonOrThrow == json"""{ "TYPE": "Accepted" }""")

  "requireIsResponding" in:
    val output = mutable.Buffer.empty[String]
    autoClosing(newTextAgentClient(Some(TestUserId -> Password))(output += _)) { client =>
      client.login() await 99.s
      client.requireIsResponding()
    }
    assert(output == List("JS7 Agent is responding"))
    val agentUri = findFreeLocalUri()
    autoClosing(new PekkoHttpAgentTextApi(agentUri, None, _ => ())) { client =>
      val t = intercept[Exception]:
        client.requireIsResponding()
      assert(t.isInstanceOf[pekko.stream.StreamTcpException]
        || t.getClass.getName.startsWith("pekko.http")
        || t.toString.contains("java.net.ConnectException"))
    }

  private def newTextAgentClient(userAndPassword: Option[UserAndPassword])(output: String => Unit) =
    new PekkoHttpAgentTextApi(agentUri = agent.localUri, userAndPassword, output,
      configDirectory = Some(configDirectory))

private object PekkoHttpAgentTextApiTest:
  private val ExpectedTerminate = AgentCommand.ShutDown(Some(SIGTERM))
  private val TestUserId = TestUserAndPassword.userId
  private val Password = TestUserAndPassword.password

  private def interceptUnauthorized(body: => Unit) =
    val e = intercept[PekkoHttpClient.HttpException]:
      body
    assert(e.status == Unauthorized)
