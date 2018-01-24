package com.sos.jobscheduler.agent.tests

import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.directives.SecurityDirectives.Authenticator
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.client.TextAgentClient
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.test.{TestAgentDirectoryProvider, TestAgentProvider}
import com.sos.jobscheduler.agent.tests.TextAgentClientTest._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.akkahttp.web.auth.OurAuthenticator
import com.sos.jobscheduler.common.auth.{HashedPassword, User, UserAndPassword, UserId}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.jobscheduler.data.agent.AgentAddress
import javax.inject.Singleton
import org.scalatest.Assertions._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author Joacim Zschimmer
 */
final class TextAgentClientTest extends FreeSpec with BeforeAndAfterAll with HasCloser with TestAgentProvider with TestAgentDirectoryProvider {

  override protected lazy val agentConfiguration = {
    val c = newAgentConfiguration()
    c.copy(
      http = None)
      .withHttpsInetSocketAddress(c.http.get.address)
  }

  override protected def extraAgentModule = new AbstractModule {
    def configure() = {}

    @Provides @Singleton
    def commandExecutor(): CommandHandler = new CommandHandler {
      def execute(command: AgentCommand, meta: CommandMeta): Future[command.Response] = {
        val response = command match {
          case ExpectedTerminate ⇒ AgentCommand.Accepted
          case _ ⇒ fail()
        }
        Future.successful(response.asInstanceOf[command.Response])
      }

      def overview = throw new NotImplementedError
      def detailed = throw new NotImplementedError
    }

    @Provides @Singleton
    def authenticator(conf: AgentConfiguration)(implicit ec: ExecutionContext): Authenticator[User] =
      new OurAuthenticator({
        case TestUserId ⇒ Some(HashedPassword(Password, identity))
        case _ ⇒ None
      })
  }

  override def afterAll() = closer closeThen { super.afterAll() }

  "Unauthorized" in {
    autoClosing(newTextAgentClient(_ ⇒ (), login = None)) { client ⇒
      interceptUnauthorized {
        client.executeCommand("""{ TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }""")
      }
    }
    autoClosing(newTextAgentClient(_ ⇒ (), Some(TestUserId → SecretString("WRONG-PASSWORD")))) { client ⇒
      interceptUnauthorized {
        client.executeCommand("""{ TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }""")
      }
    }
  }

  "AgentCommand" in {
    val output = mutable.Buffer[String]()
    autoClosing(newTextAgentClient(output += _, Some(TestUserId → Password))) { client ⇒
      client.executeCommand("""{ TYPE: Terminate, sigtermProcesses: true, sigkillProcessesAfter: 10 }""")
      client.get("")
    }
    assert(output.size == 3)
    assert(output(0) == "TYPE: Accepted")
    assert(output(1) == "---")
    assert(output(2) contains "startedAt: '2")
    assert(output(2) contains "isTerminating: false")
  }

  "requireIsResponding" in {
    val output = mutable.Buffer[String]()
    autoClosing(newTextAgentClient(output += _, Some(TestUserId → Password))) { client ⇒
      client.requireIsResponding()
    }
    assert(output == List("JobScheduler Agent is responding"))
    val agentUri = AgentAddress(s"http://127.0.0.1:${findRandomFreeTcpPort()}")
    autoClosing(new TextAgentClient(agentUri, _ ⇒ Unit)) { client ⇒
      intercept[akka.stream.StreamTcpException] {
        client.requireIsResponding()
      }
    }
  }

  private def newTextAgentClient(output: String ⇒ Unit, login: Option[UserAndPassword]) =
    new TextAgentClient(agentUri = AgentAddress(agent.localUri.toString), output, login, Some(keystoreReference))
}

private object TextAgentClientTest {
  private val ExpectedTerminate = AgentCommand.Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(10.seconds))
  private val TestUserId = UserId("SHA512-USER")
  private val Password = SecretString("SHA512-PASSWORD")

  private def interceptUnauthorized(body: ⇒ Unit) = {
    val e = intercept[TextAgentClient.HttpException] {
      body
    }
    assert(e.status == Unauthorized)
  }
}
