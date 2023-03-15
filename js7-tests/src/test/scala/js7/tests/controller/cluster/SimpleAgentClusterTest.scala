package js7.tests.controller.cluster

import js7.agent.data.commands.AgentCommand
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.node.NodeId
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.traced

final class SimpleAgentClusterTest extends ControllerClusterTester
{
  private lazy val agentPath = agentPaths.head

  "Cluster replicates journal files properly" in {
    runControllerAndBackup() { (primary, primaryController, primaryAgents, backup, backupController, backupAgents, _) =>


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

      val stampedSeq = primaryController.runOrder(FreshOrder(OrderId("🔶"), TestWorkflow.path))
      assert(stampedSeq.last.value == OrderFinished())

      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)
      assertEqualJournalFiles(primary.agents(0), backup.agents(0), n = 1)
    }
  }
}

object SimpleAgentClusterTest {
  private val logger = Logger[this.type]
}
