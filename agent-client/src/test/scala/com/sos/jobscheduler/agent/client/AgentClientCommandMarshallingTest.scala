package com.sos.jobscheduler.agent.client

import akka.util.Timeout
import com.google.inject.{AbstractModule, Provides}
import com.sos.jobscheduler.agent.client.AgentClient.{RequestTimeout, commandDurationToRequestTimeout}
import com.sos.jobscheduler.agent.client.AgentClientCommandMarshallingTest._
import com.sos.jobscheduler.agent.command.{CommandExecutor, CommandMeta}
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AbortImmediately, Terminate}
import com.sos.jobscheduler.agent.test.AgentTest
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Duration
import java.util.concurrent.TimeUnit.MILLISECONDS
import javax.inject.Singleton
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class AgentClientCommandMarshallingTest extends FreeSpec with BeforeAndAfterAll with ScalaFutures with HasCloser with AgentTest {

  override def afterAll(): Unit = {
    onClose { super.afterAll() }
    close()
  }

  override protected def extraAgentModule = new AbstractModule {
    def configure() = {}

    @Provides @Singleton
    def commandExecutor(): CommandExecutor = new CommandExecutor {
      def executeCommand(command: AgentCommand, meta: CommandMeta): Future[command.Response] =
        Future {
          (command match {
            case ExpectedTerminate ⇒ EmptyResponse
            case AbortImmediately ⇒ EmptyResponse
            case _ ⇒ throw new NotImplementedError
          })
          .asInstanceOf[command.Response]
        }
      }
  }

  override implicit val patienceConfig = PatienceConfig(timeout = 10.s.toConcurrent)
  private lazy val client = SimpleAgentClient(agentUri = agent.localUri).closeWithCloser

  "commandDurationToRequestTimeout" in {
    val upperBound = 30 * 24.h  // The upper bound depends on Akka tick length (Int.MaxValue ticks, a tick can be as short as 1ms)
    for (duration ← List[Duration](0.s, 1.s, upperBound)) {
      assert(commandDurationToRequestTimeout(duration) == Timeout((RequestTimeout + duration).toMillis, MILLISECONDS))
    }
  }

  List[(AgentCommand, AgentCommand.Response)](
    ExpectedTerminate → EmptyResponse,
    AbortImmediately → EmptyResponse)
  .foreach { case (command, response) ⇒
    command.getClass.getSimpleName in {
      whenReady(client.executeCommand(command)) { o ⇒
        assert(o == response)
      }
    }
  }
}

private object AgentClientCommandMarshallingTest {
  private val ExpectedTerminate = Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(10.s))
}
