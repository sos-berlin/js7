package com.sos.scheduler.engine.agent.process

import com.google.inject.{AbstractModule, Guice, Provides}
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{CloseProcessResponse, StartProcessResponse}
import com.sos.scheduler.engine.agent.process.ProcessCommandExecutorTest._
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.taskserver.TaskServer
import javax.inject.Singleton
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Inside.inside
import org.scalatest.Matchers._
import org.scalatest.concurrent.AsyncAssertions.Waiter
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.time.SpanSugar._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Try}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessCommandExecutorTest extends FreeSpec {
  private lazy val taskServers = List.fill(2) { mock[TaskServer] }
  private lazy val processes = AgentProcessIds zip taskServers map { case (id, taskServer) ⇒ new AgentProcess(id, taskServer) }
  private lazy val commandExecutor = Guice.createInjector(new TestModule(processes)).instance[ProcessCommandExecutor]

  "StartProcess" in {
    val command = StartSeparateProcess(controllerAddress = "127.0.0.1:9999", javaOptions = JavaOptions, javaClasspath = JavaClasspath)
    for (nextProcessId ← AgentProcessIds) {
      val response = awaitResult(commandExecutor.apply(command), 1.seconds)
      inside(response) { case StartProcessResponse(id) ⇒ id shouldEqual nextProcessId }
    }
    for (taskServer ← taskServers) {
      verify(taskServer, times(1)).start()
      verify(taskServer, never).kill()
      verify(taskServer, never).close()
    }
  }

  "CloseProcess" in {
    val commands = List(
      CloseProcess(processes(0).id, kill = false),
      CloseProcess(processes(1).id, kill = true))
    for (command ← commands) {
      val response = awaitResult(commandExecutor.apply(command), 3.seconds)
      inside(response) { case CloseProcessResponse ⇒ }
    }
    verify(taskServers(0), times(1)).start()
    verify(taskServers(0), never).kill()
    verify(taskServers(0), times(1)).close()

    verify(taskServers(1), times(1)).start()
    verify(taskServers(1), times(1)).kill()
    verify(taskServers(1), times(1)).close()
  }
}

private object ProcessCommandExecutorTest {
  private val AgentProcessIds = List(111111111111111111L, 222222222222222222L) map AgentProcessId.apply
  private val JavaOptions = "JAVA-OPTIONS"
  private val JavaClasspath = "JAVA-CLASSPATH"

  private class TestModule(processes: List[AgentProcess]) extends AbstractModule {
    private val processIterator = processes.iterator

    def configure() = {}

    @Provides @Singleton
    private def newAgentProcess: AgentProcessFactory = new AgentProcessFactory {
      def apply(command: StartProcess) = {
        inside(command) {
          case command: StartSeparateProcess ⇒
            command.javaOptions shouldEqual JavaOptions
            command.javaClasspath shouldEqual JavaClasspath
        }
        processIterator.synchronized { processIterator.next() }
      }
    }

    @Provides @Singleton
    private def newAgentProcessId: () ⇒ AgentProcessId = AgentProcessIds.synchronized { AgentProcessIds.iterator.next }
  }

  def waiterOnComplete[A](w: Waiter, future: Future[A])(pf: PartialFunction[Try[A], Unit]): Unit =
    future.onComplete(pf orElse {
      case Failure(t) ⇒ w { fail(t.toString, t) }
      case o ⇒ w { fail(s"Unexpected: $o") }
    })
}
