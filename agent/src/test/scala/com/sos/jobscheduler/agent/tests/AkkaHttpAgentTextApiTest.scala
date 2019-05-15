package com.sos.jobscheduler.agent.tests

import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import cats.data.Validated.Valid
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.client.AkkaHttpAgentTextApi
import com.sos.jobscheduler.agent.command.CommandHandler
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.tests.AkkaHttpAgentTextApiTest._
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider.TestUserAndPassword
import com.sos.jobscheduler.base.auth.{HashedPassword, SimpleUser}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.akkahttp.web.auth.OurMemoizingAuthenticator
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.core.command.CommandMeta
import javax.inject.Singleton
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertions._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class AkkaHttpAgentTextApiTest
extends FreeSpec with BeforeAndAfterAll with HasCloser with TestAgentProvider with TestAgentDirectoryProvider {

  override protected lazy val agentConfiguration = AgentConfiguration.forTest(configAndData = agentDirectory,
    httpPort = None, httpsPort = Some(findFreeTcpPort()))

  override protected def extraAgentModule = new AbstractModule {
    @Provides @Singleton
    def commandExecutor(): CommandHandler = new CommandHandler {
      def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
      Task {
        (command match {
          case ExpectedTerminate => Valid(AgentCommand.Response.Accepted)
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
    autoClosing(newTextAgentClient(_ => ())) { client =>
      interceptUnauthorized {
        client.executeCommand("""{ TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }""")
      }
    }
  }

  "Unauthorized when credentials are wrong" in {
    autoClosing(newTextAgentClient(_ => ())) { client =>
      val e = intercept[AkkaHttpClient.HttpException] {
        client.login(Some(TestUserId -> SecretString("WRONG-PASSWORD"))) await 99.s
      }
      assert(e.status == Unauthorized)
      assert(e.dataAsString contains "Login: unknown user or invalid password")
    }
  }

  "AgentCommand" in {
    val output = mutable.Buffer[String]()
    autoClosing(newTextAgentClient(output += _)) { client =>
      client.login(Some(TestUserId -> Password)) await 99.s
      client.executeCommand("""{ TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }""")
      client.getApi("")
    }
    assert(output.size == 3)
    assert(output(0) == "TYPE: Accepted")
    assert(output(1) == "---")
    assert(output(2) contains "startedAt: 15")
    assert(output(2) contains "isTerminating: false")
  }

  "requireIsResponding" in {
    val output = mutable.Buffer[String]()
    autoClosing(newTextAgentClient(output += _)) { client =>
      client.login(Some(TestUserId -> Password)) await 99.s
      client.requireIsResponding()
    }
    assert(output == List("JobScheduler Agent is responding"))
    val agentUri = s"http://127.0.0.1:${findFreeTcpPort()}"
    autoClosing(new AkkaHttpAgentTextApi(agentUri, _ => Unit)) { client =>
      val t = intercept[Exception] {
        client.requireIsResponding()
      }
      assert(t.isInstanceOf[akka.stream.StreamTcpException] || t.getClass.getName.startsWith("akka.http"))
    }
  }

  private def newTextAgentClient(output: String => Unit) =
    new AkkaHttpAgentTextApi(agentUri = agent.localUri.toString, output, configDirectory = Some(configDirectory))
}

private object AkkaHttpAgentTextApiTest {
  private val ExpectedTerminate = AgentCommand.Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(10.seconds))
  private val TestUserId = TestUserAndPassword.userId
  private val Password = TestUserAndPassword.password

  private def interceptUnauthorized(body: => Unit) = {
    val e = intercept[AkkaHttpClient.HttpException] {
      body
    }
    assert(e.status == Unauthorized)
  }
}
