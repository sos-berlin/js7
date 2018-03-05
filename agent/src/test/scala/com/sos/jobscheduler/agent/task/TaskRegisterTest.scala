package com.sos.jobscheduler.agent.task

import akka.actor.{Actor, Props, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, Akkas}
import com.sos.jobscheduler.agent.data.AgentTaskId
import com.sos.jobscheduler.agent.data.views.{TaskOverview, TaskRegisterOverview}
import com.sos.jobscheduler.agent.task.TaskRegisterTest._
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.process.ProcessSignal
import com.sos.jobscheduler.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.retryUntil
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.job.JobPath
import com.typesafe.config.ConfigFactory
import java.time.Instant.now
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.JavaConverters._
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class TaskRegisterTest extends FreeSpec with HasCloser with BeforeAndAfterAll with TestAgentDirectoryProvider {

  private implicit val timeout = Timeout(99.seconds)
  private implicit lazy val actorSystem = Akkas.newActorSystem("TaskRegisterTest",
    ConfigFactory.parseMap(Map("akka.scheduler.tick-duration" → "100 millis").asJava))  // Our default of 1s slows down this test
  TestAgentDirectoryProvider
  private implicit lazy val agentConfiguration = AgentConfiguration.forTest(Some(agentDirectory)).finishAndProvideFiles
  private implicit lazy val timerService = new TimerService(idleTimeout = Some(1.s))
  private lazy val actor = actorSystem.actorOf(TaskRegisterActor.props(agentConfiguration, timerService) )
  private lazy val handle = new TaskRegister(actor)
  private lazy val aTask = new TestTask(AgentTaskId(1, 11))
  private lazy val bTask = new TestTask(AgentTaskId(2, 22))
  private lazy val cTask = new TestTask(AgentTaskId(3, 33))

  override def afterAll() = {
    closer.close()
    super.afterAll()
  }

  "GetOverview empty" in {
    assert((handle.overview await 99.s) == TaskRegisterOverview(0, 0))
  }

  "Add" in {
    handle.add(aTask) await 99.s
    handle.add(bTask) await 99.s
    assert((handle.overview await 99.s) == TaskRegisterOverview(2, 2))
  }

  "crashKillScript (1)" in {
    assert(crashKillScript == Set(s""""$killFile" -kill-agent-task-id=1-11 -pid=123 -master-task-id=0 -job=/TEST""",
                                  s""""$killFile" -kill-agent-task-id=2-22 -pid=123 -master-task-id=0 -job=/TEST"""))
  }

  "GetTaskOverview" in {
    for (task ← Array(aTask, bTask)) {
      assert((handle.taskOverview(task.id) await 99.s) == task.overview)
    }
  }

  "SendSignalToAllProcesses" in {
    handle.sendSignalToAllProcesses(SIGTERM) await 99.s
    assert(aTask.signalled == SIGTERM)
    assert(bTask.signalled == SIGTERM)
  }

  "Remove" in {
    handle.remove(aTask.id)
  }

  "GetOverview" in {
    assert((handle.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 1, totalTaskCount = 2))
  }

  "GetTaskOverviews" in {
    assert((handle.taskOverviews await 99.s) == List(bTask.overview))
  }

  "Add (2)" in {
    handle.add(cTask) await 99.s
    assert((handle.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 2, totalTaskCount = 3))
    assert((handle.taskOverviews await 99.s).toSet == Set(bTask.overview, cTask.overview))
  }

  "crashKillScript (2)" in {
    assert(crashKillScript == Set(s""""$killFile" -kill-agent-task-id=2-22 -pid=123 -master-task-id=0 -job=/TEST""",
                                  s""""$killFile" -kill-agent-task-id=3-33 -pid=123 -master-task-id=0 -job=/TEST"""))
  }

  "Terminate" in {
    val terminated = Promise[Unit]()
    actorSystem.actorOf(Props {
      new Actor() {
        context.watch(actor)
        def receive = {
          case Terminated(`actor`) ⇒ terminated.success(())
        }
      }
    })
    val terminateResponse = (actor ? TaskRegisterActor.Command.Terminate(sigterm = true, sigkillProcessesAfter = now + 300.ms)).await(99.s)
    assert(terminateResponse == Completed)
    if (!isWindows) retryUntil(1.s, 10.ms) {
      assert(bTask.signalled == SIGTERM)
      assert(cTask.signalled == SIGTERM)
    }
    retryUntil(99.s, 10.ms) {
      assert(bTask.signalled == SIGKILL)
      assert(cTask.signalled == SIGKILL)
    }
    terminated.future await 99.s
  }

  private def crashKillScript = autoClosing(scala.io.Source.fromFile(agentConfiguration.crashKillScriptFile)) { _.getLines.toSet }

  private def killFile = agentConfiguration.killScript.get.file
}

private object TaskRegisterTest {
  private class TestTask(val id: AgentTaskId) extends BaseAgentTask {

    val jobPath = JobPath("/TEST")
    val pidOption = Some(Pid(123))
    val overview = TaskOverview(jobPath, id, pidOption, now.toTimestamp)
    val terminatedPromise = Promise[Completed]()
    var signalled: ProcessSignal = null

    def terminated = terminatedPromise.future

    def sendProcessSignal(signal: ProcessSignal) = {
      signalled = signal
      if (signal == SIGKILL) terminatedPromise.success(Completed)
    }
  }
}
