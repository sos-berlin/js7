package com.sos.jobscheduler.agent.task

import akka.actor.ActorDSL._
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, Akkas}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.task.TaskRegisterTest._
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.retryUntil
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.jobnet.JobPath
import com.typesafe.config.ConfigFactory
import java.time.Instant.now
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.JavaConversions._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
final class TaskRegisterTest extends FreeSpec with HasCloser with BeforeAndAfterAll with AgentDirectoryProvider {

  override def beforeAll() = {
    provideAgent2Directories()   // For CrashKillScript
    me.watch(actor)
    super.beforeAll()
  }

  override def afterAll() = {
    closer.close()
    super.afterAll()
  }

  private implicit lazy val actorSystem = Akkas.newActorSystem("TaskRegisterTest",
    ConfigFactory.parseMap(Map("akka.scheduler.tick-duration" → "100 millis")))  // Our default of 1s slows down this test
  AgentDirectoryProvider
  private implicit lazy val agentConfiguration = AgentConfiguration.forTest(Some(agentDirectory)).finishAndProvideFiles
  private implicit lazy val timerService = new TimerService(idleTimeout = Some(1.s))
  private implicit lazy val me = inbox()
  private val terminated = Promise[Completed]()
  private lazy val actor = actorSystem.actorOf(TaskRegister.props(terminated))
  private lazy val aTask = new TestTask(AgentTaskId(1, 11))
  private lazy val bTask = new TestTask(AgentTaskId(2, 22))
  private lazy val cTask = new TestTask(AgentTaskId(3, 33))


  "GetOverview empty" in {
    actor ! TaskRegister.Command.GetOverview
    assert(me.receive() == TaskRegister.Response.GotOverview(TaskRegisterOverview(0, 0)))
  }

  "Add" in {
    actor ! TaskRegister.Input.Add(aTask)
    actor ! TaskRegister.Input.Add(bTask)
    actor ! TaskRegister.Command.GetOverview
    assert(me.receive() == TaskRegister.Response.GotOverview(TaskRegisterOverview(2, 2)))
  }

  "crashKillScript (1)" in {
    assert(crashKillScript == Set(s""""$killFile" -kill-agent-task-id=1-11 -pid=123 -master-task-id=0 -job=/TEST""",
                                  s""""$killFile" -kill-agent-task-id=2-22 -pid=123 -master-task-id=0 -job=/TEST"""))
  }

  "GetTaskOverview" in {
    for (task ← Array(aTask, bTask)) {
      actor ! TaskRegister.Command.GetTaskOverview(task.id)
      assert(me.receive() == TaskRegister.Response.GotTaskOverview(task.overview))
    }
  }

  "SendSignalToAllProcesses" in {
    actor ! TaskRegister.Command.SendSignalToAllProcesses(SIGTERM)
    assert(me.receive() == TaskRegister.Response.OK)
    assert(aTask.signalled == SIGTERM)
    assert(bTask.signalled == SIGTERM)
  }

  "Remove" in {
    actor ! TaskRegister.Input.Remove(aTask.id)
    actor ! TaskRegister.Command.GetOverview
    assert(me.receive() == TaskRegister.Response.GotOverview(TaskRegisterOverview(currentTaskCount = 1, totalTaskCount = 2)))
    actor ! TaskRegister.Command.GetTaskOverviews
    assert(me.receive() == TaskRegister.Response.GotTaskOverviews(List(bTask.overview)))
  }

  "Add (2)" in {
    actor ! TaskRegister.Input.Add(cTask)
    actor ! TaskRegister.Command.GetOverview
    assert(me.receive() == TaskRegister.Response.GotOverview(TaskRegisterOverview(currentTaskCount = 2, totalTaskCount = 3)))
    actor ! TaskRegister.Command.GetTaskOverviews
    assert(me.receive().asInstanceOf[TaskRegister.Response.GotTaskOverviews].overviews.toSet == Set(bTask.overview, cTask.overview))
  }

  "crashKillScript (2)" in {
    assert(crashKillScript == Set(s""""$killFile" -kill-agent-task-id=2-22 -pid=123 -master-task-id=0 -job=/TEST""",
                                  s""""$killFile" -kill-agent-task-id=3-33 -pid=123 -master-task-id=0 -job=/TEST"""))
  }

  "Terminate" in {
    actor ! TaskRegister.Command.Terminate(sigterm = true, sigkillProcessesAfter = now + 300.ms)
    retryUntil(1.s, 10.ms) {
      assert(bTask.signalled == SIGTERM)
      assert(cTask.signalled == SIGTERM)
    }
    assert(me.receive() == TaskRegister.Response.OK)
    assert(bTask.signalled == SIGKILL)
    assert(cTask.signalled == SIGKILL)
  }

  private def crashKillScript = autoClosing(io.Source.fromFile(agentConfiguration.crashKillScriptFile)) { _.getLines.toList } .toSet

  private def killFile = agentConfiguration.killScript.get.file
}

private object TaskRegisterTest {
  private class TestTask(val id: AgentTaskId) extends BaseAgentTask {

    val jobPath = JobPath("/TEST")
    val pidOption = Some(Pid(123))
    val overview = TaskOverview(jobPath, id, pidOption, now)
    val terminatedPromise = Promise[Completed]()
    var signalled: ProcessSignal = null

    def terminated = terminatedPromise.future

    def sendProcessSignal(signal: ProcessSignal) = {
      signalled = signal
      if (signal == SIGKILL) terminatedPromise.success(Completed)
    }
  }
}
