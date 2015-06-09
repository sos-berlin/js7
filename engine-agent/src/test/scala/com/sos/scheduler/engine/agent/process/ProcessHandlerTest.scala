package com.sos.scheduler.engine.agent.process

import com.google.inject.{AbstractModule, Guice, Provides}
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, StartProcessResponse}
import com.sos.scheduler.engine.agent.process.ProcessHandlerTest._
import com.sos.scheduler.engine.base.process.ProcessSignal.SIGKILL
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversable
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import java.nio.file.Paths
import javax.inject.Singleton
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Inside.inside
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.time.SpanSugar._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessHandlerTest extends FreeSpec {
  private lazy val taskServers = List.fill(2) { mockTaskServer() }
  private lazy val processes = AgentProcessIds zip taskServers map { case (id, taskServer) ⇒ new AgentProcess(id, taskServer) }
  private lazy val processHandler = Guice.createInjector(new TestModule(processes)).instance[ProcessHandler]

  "StartProcess" in {
    val command = StartSeparateProcess(controllerAddress = TestControllerAddress, javaOptions = JavaOptions, javaClasspath = JavaClasspath)
    for (nextProcessId ← AgentProcessIds) {
      val response = awaitResult(processHandler.apply(command), 1.seconds)
      inside(response) { case StartProcessResponse(id) ⇒ id shouldEqual nextProcessId }
    }
    for (taskServer ← taskServers) {
      verify(taskServer, times(1)).start()
      verify(taskServer, never).sendProcessSignal(SIGKILL)
      verify(taskServer, never).close()
    }
  }

  "ProcessHandlerView" - {
    def view: ProcessHandlerView = processHandler

    "processCount" in {
      assert(view.processCount == taskServers.size)
    }

    "processes" in {
      val processMap = view.processes toKeyedMap {_.id}
      assert(processMap.size == taskServers.size)
      for (id ← AgentProcessIds) assert(processMap contains id)
      for (o ← processMap.values) assert(o.controllerAddress == TestControllerAddress)
    }
  }

  "CloseProcess" in {
    val commands = List(
      CloseProcess(processes(0).id, kill = false),
      CloseProcess(processes(1).id, kill = true))
    for (command ← commands) {
      val response = awaitResult(processHandler.apply(command), 3.seconds)
      inside(response) { case EmptyResponse ⇒ }
    }
    verify(taskServers(0), times(1)).start()
    verify(taskServers(0), never).sendProcessSignal(SIGKILL)
    verify(taskServers(0), times(1)).close()

    verify(taskServers(1), times(1)).start()
    verify(taskServers(1), times(1)).sendProcessSignal(SIGKILL)
    verify(taskServers(1), times(1)).close()
  }
}

private object ProcessHandlerTest {
  private val AgentProcessIds = List(111111111111111111L, 222222222222222222L) map AgentProcessId.apply
  private val JavaOptions = "JAVA-OPTIONS"
  private val JavaClasspath = "JAVA-CLASSPATH"
  private val TestControllerAddress = "127.0.0.1:9999"

  private def mockTaskServer() = mock[TaskServer] sideEffect { o ⇒
    // For ProcessHandler.overview
    when(o.taskStartArguments) thenReturn TaskStartArguments(controllerAddress = TestControllerAddress, directory = Paths.get(""))
  }

  private class TestModule(processes: List[AgentProcess]) extends AbstractModule {
    private val processIterator = processes.iterator

    def configure() = {}

    @Provides @Singleton
    private def agentProcessFactory: AgentProcessFactory = new AgentProcessFactory {
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
    private def newAgentProcessId: () ⇒ AgentProcessId =
      AgentProcessIds.synchronized { AgentProcessIds.iterator.next }
  }
}
