package com.sos.scheduler.engine.agent.client

import akka.util.Timeout
import com.google.inject.{AbstractModule, Provides}
import com.sos.scheduler.engine.agent.client.AgentClient.{RequestTimeout, commandDurationToRequestTimeout}
import com.sos.scheduler.engine.agent.client.AgentClientCommandMarshallingTest._
import com.sos.scheduler.engine.agent.command.{CommandExecutor, CommandMeta}
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, FileOrderSourceContent, Response}
import com.sos.scheduler.engine.agent.data.commands.{AbortImmediately, Command, RequestFileOrderSourceContent, Terminate}
import com.sos.scheduler.engine.agent.test.AgentTest
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Duration
import java.util.concurrent.TimeUnit.MILLISECONDS
import javax.inject.Singleton
import org.junit.runner.RunWith
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentClientCommandMarshallingTest extends FreeSpec with BeforeAndAfterAll with ScalaFutures with HasCloser with AgentTest {

  override def afterAll(): Unit = {
    onClose { super.afterAll() }
    close()
  }

  override protected def extraAgentModule = new AbstractModule {
    def configure() = {}

    @Provides @Singleton
    def commandExecutor(): CommandExecutor = new CommandExecutor {
      def executeCommand(command: Command, meta: CommandMeta): Future[command.Response] =
        Future {
          (command match {
            case ExpectedTerminate ⇒ EmptyResponse
            case xx ⇒ ExpectedFileOrderSourceContent
          })
          .asInstanceOf[command.Response]
        }
      }
  }

  override implicit val patienceConfig = PatienceConfig(timeout = 10.s.toConcurrent)
  private lazy val client = SimpleAgentClient(agentUri = agent.localUri).closeWithCloser

  "commandDurationToRequestTimeout" in {
    val upperBound = RequestFileOrderSourceContent.MaxDuration  // The upper bound depends on Akka tick length (Int.MaxValue ticks, a tick can be as short as 1ms)
    for (duration ← List[Duration](0.s, 1.s, upperBound)) {
      assert(commandDurationToRequestTimeout(duration) == Timeout((RequestTimeout + duration).toMillis, MILLISECONDS))
    }
  }

  List[(Command, Response)](
    ExpectedRequestFileOrderSourceContent → ExpectedFileOrderSourceContent,
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
  private val ExpectedRequestFileOrderSourceContent = RequestFileOrderSourceContent(
    directory = "DIRECTORY",
    regex = "REGEX",
    duration= 111222.ms,
    knownFiles = Set("a", "b"))
  private val ExpectedFileOrderSourceContent = FileOrderSourceContent(List(
    FileOrderSourceContent.Entry("a.txt", 23334445555L),
    FileOrderSourceContent.Entry("b.txt", 20000000000L)))
  private val ExpectedTerminate = Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(10.s))
}
