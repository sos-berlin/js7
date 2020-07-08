package js7.agent.task

import akka.actor.{Actor, Props, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import js7.common.configutils.Configs._
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.data.AgentTaskId
import js7.agent.data.views.{TaskOverview, TaskRegisterOverview}
import js7.agent.task.TaskRegisterTest._
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.generic.Completed
import js7.base.process.ProcessSignal
import js7.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.HasCloser
import js7.common.process.Processes.Pid
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.system.OperatingSystem.isWindows
import js7.common.time.WaitForCondition.retryUntil
import js7.data.job.JobKey
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.executable.WorkflowJob
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.io
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
final class TaskRegisterTest extends AnyFreeSpec with HasCloser with BeforeAndAfterAll with TestAgentDirectoryProvider {

  private implicit val timeout = Timeout(99.seconds)
  private implicit lazy val actorSystem = newAgentActorSystem("TaskRegisterTest",
    config"akka.scheduler.tick-duration = 100ms")  // Our default of 1s slows down this test
  TestAgentDirectoryProvider
  private implicit lazy val agentConfiguration = AgentConfiguration.forTest(agentDirectory).finishAndProvideFiles
  private lazy val actor = actorSystem.actorOf(TaskRegisterActor.props(agentConfiguration.killScriptConf) )
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
    assert(crashKillScript == Set(s""""$killFile" --kill-agent-task-id=1-11 --pid=123 --controller-task-id=0""",
                                  s""""$killFile" --kill-agent-task-id=2-22 --pid=123 --controller-task-id=0"""))
  }

  "GetTaskOverview" in {
    for (task <- Array(aTask, bTask)) {
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
    assert(crashKillScript == Set(s""""$killFile" --kill-agent-task-id=2-22 --pid=123 --controller-task-id=0""",
                                  s""""$killFile" --kill-agent-task-id=3-33 --pid=123 --controller-task-id=0"""))
  }

  "Terminate" in {
    val terminated = Promise[Unit]()
    actorSystem.actorOf(Props {
      new Actor() {
        context.watch(actor)
        def receive = {
          case Terminated(`actor`) => terminated.success(())
        }
      }
    })
    val terminateResponse = (actor ? TaskRegisterActor.Command.Terminate(sigterm = true, sigkillProcessesDeadline = now + 300.ms)).await(99.s)
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

  private def crashKillScript =
    autoClosing(scala.io.Source.fromFile(agentConfiguration.killScriptConf.get.crashKillScriptFile.toFile)) { _.getLines().toSet }

  private def killFile = agentConfiguration.killScript.get.file
}

private object TaskRegisterTest {
  private class TestTask(val id: AgentTaskId) extends BaseAgentTask {

    val jobKey = JobKey(WorkflowPath("/WORKFLOW") ~ "VERSION", WorkflowJob.Name("JOB"))
    val pidOption = Some(Pid(123))
    val overview = TaskOverview(jobKey, id, pidOption, Timestamp.now)
    val terminatedPromise = Promise[Completed]()
    var signalled: ProcessSignal = null

    def terminated = terminatedPromise.future

    def sendProcessSignal(signal: ProcessSignal) = {
      signalled = signal
      if (signal == SIGKILL) terminatedPromise.success(Completed)
    }
  }
}
