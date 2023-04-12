package js7.tests.controller.cluster

import js7.agent.data.commands.AgentCommand
import js7.base.log.Logger
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.controller.cluster.SimpleAgentClusterTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.traced

final class SimpleAgentClusterTest extends ControllerClusterTester
{
  private lazy val agentPath = agentPaths.head

  override protected def items = Seq(TestWorkflow, workflow)

  "Cluster replicates journal files properly" in {
    withControllerAndBackupWithoutAgents() { (primary, backup, setting) =>
      backup.runAgents() { backupAgents =>
        runControllers(primary, backup) { (primaryController, backupController) =>
          import primaryController.eventWatch.await
          val failOverOrderId = OrderId("🔸")

          primary.runAgents() { primaryAgents =>
            primaryController.api
              .executeAgentCommand(agentPath, AgentCommand.ClusterAppointNodes(
                Map(
                  NodeId("Primary") -> primary.agents.head.localUri,
                  NodeId("Backup") -> backup.agents.head.localUri),
                NodeId("Primary")))
              .await(99.s).orThrow

            primaryAgents(0).eventWatch.await[ClusterNodesAppointed]()
            primaryAgents(0).eventWatch.await[ClusterWatchRegistered]()
            primaryAgents(0).eventWatch.await[ClusterCoupled]()

            val stampedSeq = primaryController.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
            assert(stampedSeq.last.value == OrderFinished())

            assertEqualJournalFiles(primary.controller, backup.controller, n = 1)
            assertEqualJournalFiles(primary.agents(0), backup.agents(0), n = 1)

            primaryController.api.addOrder(FreshOrder(failOverOrderId, workflow.path)).await(99.s).orThrow
            await[OrderProcessingStarted](_.key == failOverOrderId)
            //TODO: await[OrderStdoutWritten](_.key == failOverOrderId)

            // Kill Agent roughly — TODO Proper fast Agent termination desired
            primaryAgents(0).actorSystem.terminate().await(99.s)
            primaryAgents(0).untilTerminated.await(99.s)
          }

          pending // FIXME
          val eventId = backupAgents(0).eventWatch.await[ClusterFailedOver]().head.eventId
          ASemaphoreJob.continue()
          backupAgents(0).eventWatch.await[OrderProcessingStarted](_.key == failOverOrderId, after = eventId)
          backupAgents(0).eventWatch.await[OrderDetachable](_.key == failOverOrderId, after = eventId)
          //FIXME backupAgents(0).eventWatch.await[OrderFinished](_.key == failOverOrderId, after = eventId)
        }
      }
    }
  }
}

object SimpleAgentClusterTest {
  private val logger = Logger[this.type]

  private val workflow = Workflow(
    WorkflowPath("MY-WORKFLOW"),
    Seq(ASemaphoreJob.execute(AgentPath("AGENT"))))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
}
