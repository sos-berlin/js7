package js7.agent.tests

import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import com.google.inject.{AbstractModule, Provides}
import javax.inject.Singleton
import js7.agent.client.AkkaHttpAgentTextApi
import js7.agent.command.CommandHandler
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.tests.AkkaHttpAgentTextApiTest._
import js7.agent.tests.TestAgentDirectoryProvider.TestUserAndPassword
import js7.base.auth.{HashedPassword, SimpleUser, UserAndPassword}
import js7.base.configutils.Configs._
import js7.base.generic.SecretString
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.HasCloser
import js7.base.web.Uri
import js7.common.akkahttp.web.auth.OurMemoizingAuthenticator
import js7.common.http.AkkaHttpClient
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.core.command.CommandMeta
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertions._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
final class AkkaHttpAgentTextApiTest
extends AnyFreeSpec with BeforeAndAfterAll with HasCloser with TestAgentProvider
{
  override protected lazy val agentConfiguration = AgentConfiguration.forTest(configAndData = agentDirectory,
    config"js7.web.server.auth.https-client-authentication = off",   // TODO Test with client certificate
    httpPort = None, httpsPort = Some(findFreeTcpPort()))

  override protected def extraAgentModule = new AbstractModule {
    @Provides @Singleton
    def commandExecutor(): CommandHandler = new CommandHandler {
      def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
      Task {
        (command match {
          case ExpectedTerminate => Right(AgentCommand.Response.Accepted)
          case _ => fail()
        })
          .map(_.asInstanceOf[command.Response])
      }

      def overview = throw new NotImplementedError
      def detailed = throw new NotImplementedError
    }

    @Provides @Singleton
    def authenticator(conf: AgentConfiguration): Authenticator[SimpleUser] =
      new OurMemoizingAuthenticator({
        case TestUserId => Some(SimpleUser(TestUserId, HashedPassword(Password, identity)))
        case _ => None
      })
  }

  override def beforeAll() = {
    provideHttpsFiles()
    super.beforeAll()
  }

  override def afterAll() = closer closeThen { super.afterAll() }

  "Unauthorized when credentials are missing" in {
    autoClosing(newTextAgentClient(None)(_ => ())) { client =>
      interceptUnauthorized {
        client.executeCommand("""{ TYPE: ShutDown, processSignal: SIGTERM }""")
      }
    }
  }

  "Unauthorized when credentials are wrong" in {
    autoClosing(newTextAgentClient(Some(TestUserId -> SecretString("WRONG-PASSWORD")))(_ => ())) { client =>
      val e = intercept[AkkaHttpClient.HttpException] {
        client.login() await 99.s
      }
      assert(e.status == Unauthorized)
      assert(e.dataAsString contains "Login: unknown user or invalid password")
    }
  }

  "AgentCommand" in {
    val output = mutable.Buffer.empty[String]
    autoClosing(newTextAgentClient(Some(TestUserId -> Password))(output += _)) { client =>
      client.login() await 99.s
      client.executeCommand("""{ TYPE: ShutDown, processSignal: SIGTERM }""")
      client.getApi("")
    }
    assert(output.size == 3)
    assert(output(0) == "TYPE: Accepted")
    assert(output(1) == "---")
    assert(output(2) contains "startedAt: ")
    assert(output(2) contains "isTerminating: false")
  }

  "requireIsResponding" in {
    val output = mutable.Buffer.empty[String]
    autoClosing(newTextAgentClient(Some(TestUserId -> Password))(output += _)) { client =>
      client.login() await 99.s
      client.requireIsResponding()
    }
    assert(output == List("JS7 JobScheduler Agent Server is responding"))
    val agentUri = Uri(s"http://127.0.0.1:${findFreeTcpPort()}")
    autoClosing(new AkkaHttpAgentTextApi(agentUri, None, _ => ())) { client =>
      val t = intercept[Exception] {
        client.requireIsResponding()
      }
      assert(t.isInstanceOf[akka.stream.StreamTcpException] || t.getClass.getName.startsWith("akka.http"))
    }
  }

  private def newTextAgentClient(userAndPassword: Option[UserAndPassword])(output: String => Unit) =
    new AkkaHttpAgentTextApi(agentUri = agent.localUri, userAndPassword, output,
      configDirectory = Some(configDirectory))
}

private object AkkaHttpAgentTextApiTest
{
  private val ExpectedTerminate = AgentCommand.ShutDown(Some(SIGTERM))
  private val TestUserId = TestUserAndPassword.userId
  private val Password = TestUserAndPassword.password

  private def interceptUnauthorized(body: => Unit) = {
    val e = intercept[AkkaHttpClient.HttpException] {
      body
    }
    assert(e.status == Unauthorized)
  }
}
