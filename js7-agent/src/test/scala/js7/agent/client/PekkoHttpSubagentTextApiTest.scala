package js7.agent.client

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.client.PekkoHttpSubagentTextApi
import js7.agent.client.PekkoHttpSubagentTextApiTest.*
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.web.AgentUris
import js7.agent.tests.TestAgentDirectoryProvider.TestUserAndPassword
import js7.agent.tests.TestAgentProvider
import js7.base.auth.UserAndPassword
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceString}
import js7.base.configutils.Configs.*
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.HasCloser
import js7.common.http.PekkoHttpClient
import js7.common.utils.FreeTcpPortFinder.{findFreeLocalUri, findFreeTcpPort}
import org.apache.pekko
import org.apache.pekko.http.scaladsl.model.StatusCodes.*
import org.scalatest.Assertions.*
import org.scalatest.BeforeAndAfterAll
import scala.collection.mutable
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class PekkoHttpSubagentTextApiTest
extends OurTestSuite, BeforeAndAfterAll, HasCloser, TestAgentProvider:

  private given IORuntime = ioRuntime
  private given ExecutionContext = ioRuntime.compute

  override protected lazy val agentConfiguration = AgentConfiguration.forTest(
    configAndData = agentDirectory,
    name = "PekkoHttpSubagentTextApiTest",
    config"""
      js7.web.server.auth.https-client-authentication = off  # TODO Test with client certificate
      js7.auth.users.${TestUserId.string}.permissions = [AgentDirector]
      js7.auth.users.${TestUserId.string}.password = "plain:${Password.string}"
      """,
    httpPort = None,
    httpsPort = Some(findFreeTcpPort()))

  override def beforeAll() =
    provideHttpsFiles()
    super.beforeAll()

  override def afterAll() =
    closer closeThen { super.afterAll() }

  "URI" in:
    autoClosing(newSubagentTestApi(None)(_ => ())): client =>
      assert(client.directorUris.agentUri == AgentUris(agent.localUri).agentUri)
      assert(client.directorUris.command == agent.localUri / "agent" / "api" / "command")

  "Unauthorized when credentials are missing" in:
    autoClosing(newSubagentTestApi(None)(_ => ())): client =>
      interceptUnauthorized:
        client.executeCommand("""{ "TYPE": "ShutDown", "processSignal": "SIGTERM" }""")

  "Unauthorized when credentials are wrong" in:
    autoClosing(newSubagentTestApi(Some(TestUserId -> SecretString("WRONG-PASSWORD")))(_ => ())): client =>
      val e = intercept[PekkoHttpClient.HttpException]:
        client.login().await(99.s)
      assert(e.status == Unauthorized)
      assert(e.dataAsString.contains("Login: unknown user or invalid password"))

  "AgentCommand" in:
    val output = mutable.Buffer.empty[String]
    autoClosing(newSubagentTestApi(Some(TestUserId -> Password))(output += _)): client =>
      client.login().await(99.s)
      client.executeCommand("""{ "TYPE": "NoOperation" }""")
      client.getApi("")
    assert(output.size == 2)
    assert(output(0).parseJsonOrThrow == json"""{ "TYPE": "Accepted" }""")

  "requireIsResponding" in:
    val output = mutable.Buffer.empty[String]
    autoClosing(newSubagentTestApi(Some(TestUserId -> Password))(output += _)): client =>
      client.login().await(99.s)
      client.requireIsResponding()
    assert(output == List("JS7 Agent is responding"))
    val agentUri = findFreeLocalUri()
    autoClosing(new PekkoHttpSubagentTextApi(agentUri, None, _ => ())): client =>
      val t = intercept[Exception]:
        client.requireIsResponding()
      assert(t.isInstanceOf[pekko.stream.StreamTcpException]
        || t.getClass.getName.startsWith("pekko.http")
        || t.toString.contains("java.net.ConnectException"))

  private def newSubagentTestApi(userAndPassword: Option[UserAndPassword])(output: String => Unit) =
    new PekkoHttpSubagentTextApi(agentUri = agent.localUri, userAndPassword, output,
      configDirectory = Some(configDirectory))

private object PekkoHttpSubagentTextApiTest:
  private val ExpectedTerminate = AgentCommand.ShutDown(Some(SIGTERM))
  private val TestUserId = TestUserAndPassword.userId
  private val Password = TestUserAndPassword.password

  private def interceptUnauthorized(body: => Unit) =
    val e = intercept[PekkoHttpClient.HttpException]:
      body
    assert(e.status == Unauthorized)
