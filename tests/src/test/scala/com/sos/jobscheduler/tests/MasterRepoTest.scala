package com.sos.jobscheduler.tests

import cats.data.Validated.Invalid
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderStdoutWritten}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand.UpdateRepo
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.util.Try

final class MasterRepoTest extends FreeSpec {
  import MasterRepoTest._

  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentRefPath))) { provider =>
      for (v <- 1 to 4)  // For each version, we use a dedicated job which echos the VersionId
        provider.agents.head.writeExecutable(ExecutablePath(s"/EXECUTABLE-V$v$sh"), s"echo /VERSION-$v/")

      provider.runAgents() { _ =>
        provider.runMaster() { master =>
          // Add Workflow
          addWorkflowAndRunOrder(master, V1, AWorkflowPath, OrderId("A"))

          // Command is rejected due to duplicate VersionId
          assert(master.executeCommandAsSystemUser(UpdateRepo(V1)).await(99.s) ==
            Invalid(Problem(s"Duplicate VersionId '${V1.string}'")))

          // Add Workflow
          addWorkflowAndRunOrder(master, V2, BWorkflowPath, OrderId("B"))

          // Change Workflow
          changeWorkflowAndRunOrder(master, V3, AWorkflowPath, OrderId("A-3"))
        }
        // Recovery
        provider.runMaster() { master =>
          // V2
          // Previously defined workflow is still known
          runOrder(master, BWorkflowPath ~ V2, OrderId("B-AGAIN"))

          // V4 - Add and use a new workflow
          addWorkflowAndRunOrder(master, V4, CWorkflowPath, OrderId("C"))

          // Change workflow
          provider.updateRepo(master, V5, testWorkflow(V5).withId(CWorkflowPath) :: Nil)

          // Delete workflow
          provider.updateRepo(master, V6, delete = CWorkflowPath :: Nil)
          assert(Try { runOrder(master, CWorkflowPath ~ V6, OrderId("B-6")) }
            .failed.get.getMessage contains s"Has been deleted: Workflow:${CWorkflowPath.string}")

          // Command is rejected due to duplicate VersionId
          assert(master.executeCommandAsSystemUser(UpdateRepo(V2)).await(99.s) ==
            Invalid(Problem(s"Duplicate VersionId '${V2.string}'")))

          // AWorkflowPath is still version V3
          runOrder(master, AWorkflowPath ~ V3, OrderId("A-3"))
          runOrder(master, BWorkflowPath ~ V2, OrderId("B-2"))
        }
      }

      def addWorkflowAndRunOrder(master: RunningMaster, versionId: VersionId, path: WorkflowPath, orderId: OrderId): Unit = {
        val order = FreshOrder(orderId, path)
        // Command will be rejected because workflow is not yet defined
        assert(master.addOrder(order).runToFuture.await(99.s) == Invalid(Problem(s"No such key 'Workflow:${path.string}'")))
        defineWorkflowAndRunOrder(master, versionId, path, orderId)
      }

      def changeWorkflowAndRunOrder(master: RunningMaster, versionId: VersionId, path: WorkflowPath, orderId: OrderId): Unit =
        defineWorkflowAndRunOrder(master, versionId, path, orderId)

      def defineWorkflowAndRunOrder(master: RunningMaster, versionId: VersionId, path: WorkflowPath, orderId: OrderId): Unit = {
        val workflow = testWorkflow(versionId)
        assert(workflow.isAnonymous)
        val order = FreshOrder(orderId, path)
        // Add Workflow
        provider.updateRepo(master, versionId, workflow.withId(path) :: Nil)
        master.addOrderBlocking(order)
        awaitOrder(master, order.id, path ~ versionId)
      }

      def runOrder(master: RunningMaster, workflowId: WorkflowId, orderId: OrderId): Unit = {
        val order = FreshOrder(orderId, workflowId.path)
        master.addOrderBlocking(order)
        awaitOrder(master, orderId, workflowId)
      }

      def awaitOrder(master: RunningMaster, orderId: OrderId, workflowId: WorkflowId): Unit = {
        val orderAdded: OrderAdded = master.eventWatch.await[OrderAdded](_.key == orderId).head.value.event
        assert(orderAdded.workflowId == workflowId)
        val written = master.eventWatch.await[OrderStdoutWritten](_.key == orderId).head.value.event
        assert(written.chunk contains s"/VERSION-${workflowId.versionId.string}/")
        master.eventWatch.await[OrderFinished](_.key == orderId)
      }
    }
  }
}

object MasterRepoTest {
  private val AWorkflowPath = WorkflowPath("/A")
  private val BWorkflowPath = WorkflowPath("/B")
  private val CWorkflowPath = WorkflowPath("/C")
  private val V1 = VersionId("1")
  private val V2 = VersionId("2")
  private val V3 = VersionId("3")
  private val V4 = VersionId("4")
  private val V5 = VersionId("5")
  private val V6 = VersionId("6")
  private val TestAgentRefPath = AgentRefPath("/AGENT")

  private def testWorkflow(versionId: VersionId) = Workflow.of(
    Execute(WorkflowJob(TestAgentRefPath, ExecutablePath(s"/EXECUTABLE-V${versionId.string}$sh"))))
}
