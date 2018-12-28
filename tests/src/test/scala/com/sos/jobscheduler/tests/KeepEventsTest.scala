package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension ⇒ sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderFinished}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.tests.DirectoryProvider.script
import com.sos.jobscheduler.tests.KeepEventsTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
final class KeepEventsTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(TestAgentId.path :: Nil)) { directoryProvider ⇒
      for ((_, tree) ← directoryProvider.agentToTree) {
        tree.writeExecutable(TestExecutablePath, script(0.s))
      }
      directoryProvider.master.writeJson(TestWorkflow)

      RunningAgent.run(directoryProvider.agents.head.conf) { agent ⇒
        RunningMaster.runForTest(directoryProvider.master.directory) { master ⇒
          master.eventWatch.await[MasterEvent.MasterReady]()
          master.addOrderBlocking(TestOrder)
          master.eventWatch.await[OrderFinished](predicate = _.key == TestOrder.id)
        }
        agent.terminate() await 99.s
      }

      def masterJournalCount = JournalFiles.listJournalFiles(directoryProvider.master.data / "state" / "master").length
      def agentJournalCount = JournalFiles.listJournalFiles(directoryProvider.agents(0).data / "state" / "master-Master").length
      assert(masterJournalCount == 2)
      assert(agentJournalCount == 2)

      RunningAgent.run(directoryProvider.agents.head.conf) { agent ⇒
        RunningMaster.runForTest(directoryProvider.master.directory) { master ⇒
          val finished = master.eventWatch.await[OrderFinished](predicate = _.key == TestOrder.id)
          assert(finished.size == 1)
          assert(masterJournalCount == 2)
          assert(agentJournalCount == 2)

          master.executeCommandAsSystemUser(MasterCommand.KeepEvents(finished.head.eventId)).await(99.s).orThrow
          assert(masterJournalCount == 1)

          // Master sends KeepOrder after some events from Agent have arrived. So we start an order.
          master.addOrderBlocking(TestOrder)
          val added2 = master.eventWatch.await[OrderAdded](after = finished.head.eventId, predicate = _.key == TestOrder.id)
          val finished2 = master.eventWatch.await[OrderFinished](after = finished.head.eventId, predicate = _.key == TestOrder.id)
          // Master send AgentCommand.KeepEvents asynchronously, not waiting for response. So we wait a little.
          waitForCondition(5.s, 10.ms) { agentJournalCount == 1 }
          assert(agentJournalCount == 1)

          // Start a new journal file, then KeepEvent, then fetch events
          master.executeCommandAsSystemUser(MasterCommand.KeepEvents(added2.head.eventId)).await(99.s).orThrow
          master.executeCommandAsSystemUser(MasterCommand.TakeSnapshot).await(99.s).orThrow
          master.executeCommandAsSystemUser(MasterCommand.KeepEvents(finished2.head.eventId)).await(99.s).orThrow
          intercept[TimeoutException] {
            master.eventWatch.await[OrderEvent](after = finished2.head.eventId, timeout = Duration.Zero)
          }
        }
        agent.terminate() await 99.s
      }
    }
  }
}

private object KeepEventsTest {
  private val TestAgentId = AgentPath("/agent-111") % "(initial)"
  private val TestExecutablePath = ExecutablePath(s"/TEST$sh")
  private val TestWorkflow = Workflow.of(WorkflowPath("/test") % VersionId.Anonymous,
    Execute(WorkflowJob(TestAgentId.path, TestExecutablePath)))
  private val TestOrder = FreshOrder(OrderId("TEST"), TestWorkflow.id.path)
}
