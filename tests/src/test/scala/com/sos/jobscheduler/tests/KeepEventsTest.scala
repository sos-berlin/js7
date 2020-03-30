package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderFinished}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.events.MasterEvent
import com.sos.jobscheduler.tests.KeepEventsTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
final class KeepEventsTest extends FreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, TestWorkflow :: Nil, testName = Some("KeepEventsTest"))) { provider =>
      for ((_, tree) <- provider.agentToTree) {
        tree.writeExecutable(TestExecutablePath, script(0.s))
      }

      RunningAgent.run(provider.agents.head.conf) { agent =>
        provider.runMaster() { master =>
          master.eventWatch.await[MasterEvent.MasterReady]()
          master.addOrderBlocking(TestOrder)
          master.eventWatch.await[OrderFinished](predicate = _.key == TestOrder.id)
        }
        agent.terminate() await 99.s
      }

      def masterJournalFiles = JournalFiles.listJournalFiles(provider.master.dataDir / "state" / "master")
      def agentJournalFiles = JournalFiles.listJournalFiles(provider.agents(0).dataDir / "state" / "master-Master")
      assert(masterJournalFiles.size == 2)
      assert(agentJournalFiles.size == 2)

      provider.runAgents() { _ =>
        provider.runMaster() { master =>
          val finished = master.eventWatch.await[OrderFinished](predicate = _.key == TestOrder.id)
          assert(finished.size == 1)
          assert(masterJournalFiles.size == 3)
          assert(agentJournalFiles.size == 3)

          master.executeCommandAsSystemUser(MasterCommand.KeepEvents(finished.head.eventId)).await(99.s).orThrow
          assert(masterJournalFiles.size == 2)

          // Master sends KeepOrder after some events from Agent have arrived. So we start an order.
          master.addOrderBlocking(TestOrder)
          val added2 = master.eventWatch.await[OrderAdded](after = finished.head.eventId, predicate = _.key == TestOrder.id)
          val finished2 = master.eventWatch.await[OrderFinished](after = finished.head.eventId, predicate = _.key == TestOrder.id)
          // Master send AgentCommand.KeepEvents asynchronously, not waiting for response. So we wait a little.
          waitForCondition(5.s, 10.ms) { agentJournalFiles.size == 1 }
          assert(agentJournalFiles.size == 1)

          // Start a new journal file, then KeepEvent, then fetch events
          master.executeCommandAsSystemUser(MasterCommand.KeepEvents(added2.head.eventId)).await(99.s).orThrow
          master.executeCommandAsSystemUser(MasterCommand.TakeSnapshot).await(99.s).orThrow
          master.executeCommandAsSystemUser(MasterCommand.KeepEvents(finished2.head.eventId)).await(99.s).orThrow
          intercept[TimeoutException] {
            master.eventWatch.await[OrderEvent](after = finished2.head.eventId, timeout = Duration.Zero)
          }
        }
      }
    }
  }
}

private object KeepEventsTest {
  private val TestAgentRefPath = AgentRefPath("/agent-111")
  private val TestExecutablePath = ExecutablePath(s"/TEST$sh")
  private val TestWorkflow = Workflow.of(WorkflowPath("/test"),
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)))
  private val TestOrder = FreshOrder(OrderId("TEST"), TestWorkflow.id.path)
}
