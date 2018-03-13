package com.sos.jobscheduler.agent.client

import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.client.AgentClientCommandMarshallingTest._
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandMeta}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{EmergencyStop, Accepted, Terminate}
import com.sos.jobscheduler.agent.test.AgentTest
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentAddress
import javax.inject.Singleton
import org.scalatest.FreeSpec
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class AgentClientCommandMarshallingTest
extends FreeSpec with ScalaFutures with AgentTest {

  override protected def extraAgentModule = new AbstractModule {
    def configure() = {}

    @Provides @Singleton
    def commandHandler(): CommandHandler = new CommandHandler {
      def execute(command: AgentCommand, meta: CommandMeta): Future[command.Response] =
        Future {
          (command match {
            case ExpectedTerminate ⇒ Accepted
            case EmergencyStop ⇒ Accepted
            case _ ⇒ throw new NotImplementedError
          })
          .asInstanceOf[command.Response]
        }

      def overview = throw new NotImplementedError
      def detailed = throw new NotImplementedError
    }
  }
  override implicit val patienceConfig = PatienceConfig(timeout = 10.s.toConcurrent)
  private lazy val client = SimpleAgentClient(agentUri = AgentAddress(agent.localUri.toString)).closeWithCloser

  List[(AgentCommand, AgentCommand.Response)](
    ExpectedTerminate → Accepted,
    EmergencyStop → Accepted)
  .foreach { case (command, response) ⇒
    command.getClass.simpleScalaName in {
      whenReady(client.executeCommand(command)) { o ⇒
        assert(o == response)
      }
    }
  }
}

private object AgentClientCommandMarshallingTest {
  private val ExpectedTerminate = Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(123.seconds))
}
