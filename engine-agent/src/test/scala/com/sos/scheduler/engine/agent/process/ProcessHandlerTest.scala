package com.sos.scheduler.engine.agent.process

import com.google.inject.{AbstractModule, Guice, Provides}
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, StartProcessResponse}
import com.sos.scheduler.engine.agent.process.ProcessHandlerTest._
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversable
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import java.nio.file.Paths
import javax.inject.Singleton
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Inside.inside
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.time.SpanSugar._
import scala.concurrent.Promise

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessHandlerTest extends FreeSpec {

  "Start and close process cycle" - {
    lazy val testContext = new TestContext
    import testContext.{processHandler, processes, taskServers}

    "StartProcess" in {
      assert(!processHandler.terminated.isCompleted)
      for (nextProcessId ← AgentProcessIds) {
        val response = awaitResult(processHandler.apply(TestStartSeparateProcess), 3.seconds)
        inside(response) { case StartProcessResponse(id) ⇒ id shouldEqual nextProcessId }
      }
      for (o ← taskServers) {
        assert(o.started)
        assert(!o.sigkilled)
        assert(!o.sigtermed)
        assert(!o.closed)
      }
    }

    "ProcessHandlerView" - {
      def view: ProcessHandlerView = processHandler

      "currentProcessCount" in {
        assert(view.currentProcessCount == taskServers.size)
      }

      "totalProcessCount" in {
        assert(view.totalProcessCount == taskServers.size)
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
      assert(taskServers(0).started)
      assert(!taskServers(0).sigkilled)
      assert(!taskServers(0).sigtermed)
      assert(taskServers(0).closed)

      assert(taskServers(1).started)
      assert(taskServers(1).sigkilled)
      assert(!taskServers(1).sigtermed)
      assert(taskServers(1).closed)
      assert(!processHandler.terminated.isCompleted)
    }

    "ProcessHandlerView, after processes are closed" - {
      def view: ProcessHandlerView = processHandler

      "currentProcessCount" in {
        assert(view.currentProcessCount == 0)
      }

      "totalProcessCount" in {
        assert(view.totalProcessCount == taskServers.size)
      }

      "processes" in {
        assert(view.processes.isEmpty)
      }
    }
  }

  "Terminate" - {
    "When no processes are registered, ProcessHandler terminates immediately" in {
      val testContext = new TestContext
      import testContext.processHandler
      assert(!processHandler.terminated.isCompleted)
      awaitResult(processHandler.apply(Terminate(sigtermProcesses = false, sigkillProcessesAfter = 1.ms)), 3.seconds)
      awaitResult(processHandler.terminated, 3.s)
    }

    "When a process is registered, ProcessHandler terminates after the process has terminated" in {
      val testContext = new TestContext
      import testContext.{processHandler, processes, taskServers}
      for (_ ← processes) awaitResult(processHandler.apply(TestStartSeparateProcess), 3.seconds)
      assert(processHandler.totalProcessCount == processes.size)
      assert(processHandler.currentProcessCount == processes.size)
      assert(!processHandler.terminated.isCompleted)
      for (o ← taskServers) assert(!o.sigtermed)
      awaitResult(processHandler.apply(Terminate(sigtermProcesses = true, sigkillProcessesAfter = 2.s)), 3.seconds)
      for (o ← taskServers) assert(o.sigtermed)
      sleep(1.s)
      assert(!processHandler.terminated.isCompleted)
      // Now, (mocked) processes are terminated with SIGKILL
      awaitResult(processHandler.terminated, 3.s)
      assert(processHandler.currentProcessCount == processes.size)   // Because no CloseProcess was issued
    }
  }
}

private object ProcessHandlerTest {
  private val AgentProcessIds = List(111111111111111111L, 222222222222222222L) map AgentProcessId.apply
  private val JavaOptions = "JAVA-OPTIONS"
  private val JavaClasspath = "JAVA-CLASSPATH"
  private val TestControllerAddress = "127.0.0.1:9999"
  private val TestStartSeparateProcess = StartSeparateProcess(controllerAddress = TestControllerAddress, javaOptions = JavaOptions, javaClasspath = JavaClasspath)

  private class TestContext {
    val taskServers = List.fill(2) { new MockTaskServer }
    val processes = AgentProcessIds zip taskServers map { case (id, taskServer) ⇒ new AgentProcess(id, taskServer) }
    val processHandler = Guice.createInjector(new TestModule(processes)).instance[ProcessHandler]
  }

  private class MockTaskServer extends TaskServer {
    val terminatedPromise = Promise[Unit]()
    var sigtermed = false
    var sigkilled = false
    var started = false
    var closed = false

    def terminated = terminatedPromise.future

    def taskStartArguments = TaskStartArguments(controllerAddress = TestControllerAddress, directory = Paths.get(""))   // For ProcessHandler.overview

    def sendProcessSignal(signal: ProcessSignal) = signal match {
        case SIGTERM ⇒ sigtermed = true
        case SIGKILL ⇒ sigkilled = true; terminatedPromise.success(())
    }

    def start() = {
      assert(!started && !closed)
      started = true
    }

    def close() = {
      assert(started && !closed)
      closed = true
    }
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
