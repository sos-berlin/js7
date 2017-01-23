package com.sos.scheduler.engine.agent.task

import akka.actor.{Actor, ActorSystem, Props}
import akka.util.ByteString
import com.google.inject.{Guice, Provides}
import com.sos.scheduler.engine.agent.command.CommandMeta
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, StartTaskResponse}
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.views.TaskHandlerOverview
import com.sos.scheduler.engine.agent.task.TaskHandlerTest._
import com.sos.scheduler.engine.base.exceptions.PublicException
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversable
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.soslicense.{LicenseKeyBunch, LicenseKeyParameterIsMissingException}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.job.{JobPath, TaskId}
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.TaskServer.Terminated
import com.sos.scheduler.engine.taskserver.data.TaskServerArguments
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.TunnelHandle
import java.net.InetAddress
import java.time.{Duration, Instant}
import javax.inject.Singleton
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Inside.inside
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.{ExecutionContext, Promise}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TaskHandlerTest extends FreeSpec {

  "Second StartApiTask without a license is rejected - JS-1482" in {
    val testContext = new TestContext
    import testContext.{taskHandler, taskServers, tasks}
    def startTask() = awaitResult(taskHandler.execute(TestStartApiTask), 3.s)
    startTask()
    taskServers(0).mockedTerminate()
    intercept[LicenseKeyParameterIsMissingException] { startTask() }
    awaitResult(taskHandler.execute(CloseTask(tasks(0).id, kill = false)), 3.s)
    startTask()
  }

  "Start and close task cycle" - {
    lazy val testContext = new TestContext
    import testContext.{taskHandler, taskServers, tasks}

    "StartApiTask" in {
      assert(!taskHandler.terminated.isCompleted)
      for (nextAgentTaskId ← AgentTaskIds) {
        val response = awaitResult(taskHandler.execute(TestStartApiTask, CommandMeta(licenseKeyBunch = TestLicenseKeyBunch)), 3.s)
        inside(response) { case StartTaskResponse(id, TestTunnelToken) ⇒ id shouldEqual nextAgentTaskId }
      }
      for (o ← taskServers) {
        assert(o.started)
        assert(!o.sigkilled)
        assert(!o.sigtermed)
        assert(!o.closed)
      }
    }

    "TaskHandlerView" - {
      def view: TaskHandlerOverview = taskHandler.overview

      "currentTaskCount" in {
        assert(view.currentTaskCount == taskServers.size)
      }

      "totalTaskCount" in {
        assert(view.totalTaskCount == taskServers.size)
      }
    }

    "tasks" in {
      val taskMap = taskHandler.taskOverviews toKeyedMap { _.id }
      assert(taskMap.size == taskServers.size)
      for (id ← AgentTaskIds) assert(taskMap contains id)
    }

    "taskOverview" in {
      assert(taskHandler.taskOverviews.size == AgentTaskIds.size)
      assert(taskHandler.taskOverviews.toSet == (AgentTaskIds map taskHandler.taskOverview).toSet)
    }

    "CloseTask" in {
      taskServers(0).mockedTerminate()
      val commands = List(
        CloseTask(tasks(0).id, kill = false),
        CloseTask(tasks(1).id, kill = true))
      for (command ← commands) {
        val response = awaitResult(taskHandler.execute(command), 3.s)
        assert(response == EmptyResponse)
      }
      assert(taskServers(0).started)
      assert(!taskServers(0).sigkilled)
      assert(!taskServers(0).sigtermed)
      assert(taskServers(0).closed)

      assert(taskServers(1).started)
      assert(taskServers(1).sigkilled)
      assert(!taskServers(1).sigtermed)
      assert(taskServers(1).closed)
      assert(!taskHandler.terminated.isCompleted)
    }

    "TaskHandlerView, after tasks are closed" - {
      def view: TaskHandlerOverview = taskHandler.overview

      "currentTaskCount" in {
        assert(view.currentTaskCount == 0)
      }

      "totalTaskCount" in {
        assert(view.totalTaskCount == taskServers.size)
      }

      "tasks" in {
        assert(taskHandler.taskOverviews.isEmpty)
      }
    }
  }

  "Terminate" - {
    "When no tasks are registered, TaskHandler terminates immediately" in {
      val testContext = new TestContext
      import testContext.taskHandler
      assert(!taskHandler.terminated.isCompleted)
      awaitResult(taskHandler.execute(Terminate(sigtermProcesses = false)), 3.s)
      awaitResult(taskHandler.terminated, 3.s)
    }

    "When a process is registered, TaskHandler terminates after the task has terminated" in {
      val testContext = new TestContext
      import testContext.{taskHandler, taskServers, tasks}
      for (_ ← tasks) awaitResult(taskHandler.execute(TestStartApiTask, CommandMeta(licenseKeyBunch = TestLicenseKeyBunch)), 3.s)
      assert(taskHandler.overview.totalTaskCount == tasks.size)
      assert(taskHandler.overview.currentTaskCount == tasks.size)
      assert(!taskHandler.isTerminating)
      assert(!taskHandler.terminated.isCompleted)
      for (o ← taskServers) assert(!o.sigtermed)
      awaitResult(taskHandler.execute(Terminate(sigtermProcesses = true, sigkillProcessesAfter = Some(2.s))), 3.s)
      intercept[PublicException] { awaitResult(taskHandler.execute(TestStartApiTask, CommandMeta(licenseKeyBunch = TestLicenseKeyBunch)), 3.s) }
      assert(taskHandler.isTerminating)
      for (o ← taskServers) assert(o.sigtermed == !isWindows)
      sleep(1.s)
      assert(!taskHandler.terminated.isCompleted)
      // Now, (mocked) tasks are terminated with SIGKILL
      awaitResult(taskHandler.terminated, 3.s)
      assert(taskHandler.overview.currentTaskCount == tasks.size)   // Because no CloseTask was issued
    }
  }
}

private object TaskHandlerTest {
  private val AgentTaskIds = List("1-1", "2-2") map AgentTaskId.apply
  private val JavaOptions = "JAVA-OPTIONS"
  private val JavaClasspath = "JAVA-CLASSPATH"
  private val TestMasterPort = 9999
  private val TestStartApiTask = StartApiTask(javaOptions = JavaOptions, javaClasspath = JavaClasspath,
    meta = Some(StartTask.Meta(taskId = TaskId(1), job = JobPath("/test-job"))))
  private val TestLicenseKeyBunch = LicenseKeyBunch("SOS-DEMO-1-D3Q-1AWS-ZZ-ITOT9Q6")
  private val TestTunnelToken = TunnelToken(TunnelId("1"), SecretString("SECRET"))

  private class TestContext {
    val taskServers = List.fill(AgentTaskIds.size) { new MockTaskServer }
    val tasks = AgentTaskIds zip taskServers map {
      case (id_, taskServer_) ⇒ new AgentTask {
        def id = id_
        def startMeta = TestStartApiTask.meta.get
        def tunnel = mockTunnelHandle()
        def taskServer = taskServer_
        def taskArgumentsFuture = NoFuture
        def taskReleaseFuture = NoFuture
        def close() = closeTunnelAndTaskServer()
      }
    }
    val taskHandler = Guice.createInjector(new TestModule(tasks)).instance[TaskHandler]
  }

  private def mockTunnelHandle() = new TunnelHandle {
    val connectorHandler = ActorSystem("TaskHandlerTest").actorOf(Props { new Actor { def receive = { case _ ⇒ }}})
    def tunnelToken = TestTunnelToken
    def startedByHttpIpOption = None
    def connected = Promise().future
    def peerAddress = () ⇒ None
    def close() = {}
    def onInactivity(callback: Instant ⇒ Unit) = {}
    def heartbeatService = ???
    def request(request: ByteString, timeout: Option[Duration]) = ???
    override def view = ???
  }

  private class MockTaskServer extends TaskServer {
    private val terminatedPromise = Promise[Terminated.type]()
    var sigtermed = false
    var sigkilled = false
    var started = false
    var closed = false

    def terminated = terminatedPromise.future

    val arguments = TaskServerArguments.forTest(tcpPort = TestMasterPort)   // For TaskHandler.overview

    def sendProcessSignal(signal: ProcessSignal) = signal match {
      case SIGTERM ⇒
        sigtermed = true
      case SIGKILL ⇒
        sigkilled = true
        terminatedPromise.success(Terminated)
    }

    def deleteLogFiles() = {}

    def start() = {
      assert(!started && !closed)
      started = true
    }

    def close() = {
      if (!closed) {
        assert(started)
        closed = true
      }
    }

    def mockedTerminate(): Unit = terminatedPromise.trySuccess(Terminated)

    def pidOption = None
  }

  private class TestModule(tasks: List[AgentTask]) extends ScalaAbstractModule {
    private val taskIterator = tasks.iterator

    def configure() = {
      bindInstance[TimerService](TimerService(idleTimeout = Some(1.s))(ExecutionContext.global))
      bindInstance[ExecutionContext](ExecutionContext.global)
    }

    @Provides @Singleton
    private def agentTaskFactory: AgentTaskFactory = new AgentTaskFactory {
      def apply(command: StartTask, clientIp: Option[InetAddress]) = {
        inside(command) {
          case command: StartApiTask ⇒
            command.javaOptions shouldEqual JavaOptions
            command.javaClasspath shouldEqual JavaClasspath
        }
        taskIterator.synchronized { taskIterator.next() }
      }
    }

    @Provides @Singleton
    private def newAgentTaskId: () ⇒ AgentTaskId =
      AgentTaskIds.synchronized { AgentTaskIds.iterator.next }

    @Provides @Singleton
    private def agentConfiguration(): AgentConfiguration = AgentConfiguration.forTest()
  }
}
