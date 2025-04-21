package js7.tests.cluster.agent

import cats.effect.ResourceIO
import js7.agent.TestAgent
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.useSync
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.cluster.ClusterEvent.{ClusterActiveNodeShutDown, ClusterCoupled, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.NumericConstant
import js7.tests.cluster.agent.AgentShutdownAndRecoupleClusterTest.*
import js7.tests.cluster.controller.ControllerClusterTester
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.testenv.DirectorEnv

final class AgentShutdownAndRecoupleClusterTest extends ControllerClusterTester:

  protected override val agentPaths = Nil

  private lazy val subagentItems = Seq(
    SubagentItem(SubagentId("SUBAGENT-0"), agentPath, findFreeLocalUri()),
    SubagentItem(SubagentId("SUBAGENT-1"), agentPath, findFreeLocalUri()))

  override protected def items =
    Seq(
      TestWorkflow,
      AgentRef(agentPath, subagentIds),
      SubagentBundle(
        SubagentBundleId("SUBAGENT-BUNDLE"),
        Map(
          subagentIds(0) -> NumericConstant(2),
          subagentIds(1) -> NumericConstant(1)))
    ) ++ subagentItems

  "Cluster replicates journal files properly" in:
    withControllerAndBackupWithoutAgents(): (primary, backup, _) =>
      runControllers(primary, backup): (_, _) =>
        def directorEnvResource(index: Int): ResourceIO[DirectorEnv] =
          primary.directorEnvResource(subagentItems(index),
            otherSubagentIds = Seq(subagentItems(1 - index).id),
            isClusterBackup = index > 0)

        directorEnvResource(0).useSync(99.s): primaryEnv =>
          directorEnvResource(1).useSync(99.s): backupEnv =>
            backupEnv.testAgentResource.useSync(99.s): backupDirector =>
              primaryEnv.testAgentResource.useSync(99.s): primaryDirector =>
                primaryDirector.eventWatch.awaitNext[ClusterNodesAppointed]()
                primaryDirector.eventWatch.awaitNext[ClusterWatchRegistered]()
                primaryDirector.eventWatch.awaitNext[ClusterCoupled]()
                primaryDirector.terminate().await(99.s)
                backupDirector.terminate().await(99.s)

            backupEnv.testAgentResource.useSync(99.s): backupDirector =>
              primaryEnv.testAgentResource.useSync(99.s): primaryDirector =>
                primaryDirector.eventWatch.awaitNext[ClusterActiveNodeShutDown]()
                primaryDirector.eventWatch.awaitNext[ClusterCoupled]()


object AgentShutdownAndRecoupleClusterTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentIds = Seq(
    SubagentId("SUBAGENT-0"),
    SubagentId("SUBAGENT-1"))

