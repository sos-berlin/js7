package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.event.journal.JournalFiles
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.tests.DirectoryProvider.jobJson
import com.sos.jobscheduler.tests.KeepEventsTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class KeepEventsTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(TestAgentId.path :: Nil)) { directoryProvider ⇒
      for ((_, tree) ← directoryProvider.agentToTree) {
        tree.file(TestJobPath, SourceType.Json).contentString = jobJson()
      }
      directoryProvider.master.writeJson(TestWorkflow.withoutVersion)

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
      assert(masterJournalCount == 1)
      assert(agentJournalCount == 1)

      RunningAgent.run(directoryProvider.agents.head.conf) { agent ⇒
        RunningMaster.runForTest(directoryProvider.master.directory) { master ⇒
          val finished = master.eventWatch.await[OrderFinished](predicate = _.key == TestOrder.id)
          assert(masterJournalCount == 2)
          assert(agentJournalCount == 2)
          master.executeCommandAsSystemUser(MasterCommand.KeepEvents(finished.head.eventId)) await 99.s
          assert(masterJournalCount == 1)

          // Master sends KeepOrder after some events from Agent have arrived. So we start an order.
          master.addOrderBlocking(TestOrder)
          master.eventWatch.await[OrderFinished](after = finished.head.eventId, predicate = _.key == TestOrder.id)
          // Master send AgentCommand.KeepEvents asynchronously, not waiting for response. So we wait a little.
          waitForCondition(5.s, 100.ms) { agentJournalCount == 1 }
          assert(agentJournalCount == 1)
        }
        agent.terminate() await 99.s
      }
    }
  }
}

private object KeepEventsTest {
  private val TestAgentId = AgentPath("/agent-111") % "(initial)"
  private val TestJobPath = JobPath("/test")
  private val TestWorkflow = Workflow.of(WorkflowPath("/test") % "(initial)",
    Job(TestJobPath, TestAgentId.path))
  private val TestOrder = FreshOrder(OrderId("TEST"), TestWorkflow.id.path)
}
