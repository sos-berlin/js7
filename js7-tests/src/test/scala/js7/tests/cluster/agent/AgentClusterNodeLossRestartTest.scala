package js7.tests.cluster.agent

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.data.commands.AgentCommand
import js7.agent.{RunningAgent, TestAgent}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.common.auth.SecretStringGenerator
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentRefStateEvent.{AgentClusterWatchConfirmationRequired, AgentMirroredEvent}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterPassiveLost}
import js7.data.cluster.ClusterTiming
import js7.data.cluster.ClusterWatchProblems.ClusterNodeLossNotConfirmedProblem
import js7.data.node.NodeId
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.tests.cluster.agent.AgentClusterNodeLossRestartTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.{DirectorEnv, DirectoryProviderForScalaTest}

/** Restart active Agent node while passive node loss requires user confirmation.
 * <p>
 * `nodeToLossNotConfirmedProblem` should be reset at restart (or at least before recoupling). */
final class AgentClusterNodeLossRestartTest extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  private final val testHeartbeatLossPropertyKey = "js7.TEST." + SecretStringGenerator.randomString()
  private final val testAckLossPropertyKey = "js7.TEST." + SecretStringGenerator.randomString()
  sys.props(testHeartbeatLossPropertyKey) = "false"
  sys.props(testAckLossPropertyKey) = "false"

  override def agentConfig = config"""
    js7.journal.cluster.heartbeat = ${clusterTiming.heartbeat}
    js7.journal.cluster.heartbeat-timeout = ${clusterTiming.heartbeatTimeout}
    js7.journal.cluster.TEST-HEARTBEAT-LOSS = "$testHeartbeatLossPropertyKey"
    js7.journal.cluster.TEST-ACK-LOSS = "$testAckLossPropertyKey"
    """

  protected override val agentPaths = Nil

  private lazy val subagentItems =
    for subagentId <- subagentIds yield SubagentItem(subagentId, agentPath, findFreeLocalUri())

  override protected def items = Seq(agentRef) ++ subagentItems

  "test" in:
    def allocateDirectorEnv(director: SubagentItem, otherDirectorId: SubagentId, backup: Boolean = false)
    : Allocated[IO, DirectorEnv] =
      directoryProvider
        .directorEnvResource(director, otherSubagentIds = Seq(otherDirectorId),
          isClusterBackup = backup)
        .toAllocated
        .await(99.s)

    def startDirector(directorEnv: Allocated[IO, DirectorEnv]): TestAgent =
      TestAgent(directorEnv.allocatedThing.directorResource.toAllocated.await(99.s))

    val primaryDirectorEnvAllocated = allocateDirectorEnv(subagentItems(0), subagentItems(1).id)
    var primaryDirector = startDirector(primaryDirectorEnvAllocated)

    val backupDirectorAllocated = allocateDirectorEnv(subagentItems(1), subagentItems(0).id, backup = true)
      .allocatedThing.directorResource.toAllocated.await(99.s)

    directoryProvider.runController(): controller =>
      controller.eventWatch.await[AgentMirroredEvent]:
        _.event.keyedEvent.event.isInstanceOf[ClusterCoupled]

    TestAgent(backupDirectorAllocated).useSync(99.s) { backupDirector =>
      // Suppress acknowledges heartbeat, simulating a connection loss between the cluster nodes
      logger.info("💥 Break connection between cluster nodes 💥")
      sys.props(testHeartbeatLossPropertyKey) = "true"
      sys.props(testAckLossPropertyKey) = "true"
      sleep(clusterTiming.activeLostTimeout + 1.s)
      // Now, both cluster nodes require confirmation for their ClusterNodeLostEvent

      directoryProvider.runController(): controller =>
        // The newly started, untaught AgentDriver's ClusterWatch cannot decide to confirm:
        controller.eventWatch.await[AgentClusterWatchConfirmationRequired]: ke =>
          ke.key == agentPath && ke.event.problem.fromNodeId == NodeId("Primary")

        controller.eventWatch.await[AgentClusterWatchConfirmationRequired]: ke =>
          ke.key == agentPath && ke.event.problem.fromNodeId == NodeId("Backup")

        val nodeToClusterWatchConfirmationRequired =
          controller.controllerState().keyTo(AgentRefState)(agentPath).nodeToLossNotConfirmedProblem
        logger.info(s"nodeToLossNotConfirmedProblem=$nodeToClusterWatchConfirmationRequired")

        assert(nodeToClusterWatchConfirmationRequired(NodeId.primary) ==
          ClusterNodeLossNotConfirmedProblem(
            NodeId.primary, ClusterPassiveLost(NodeId.backup)))
        assert(nodeToClusterWatchConfirmationRequired(NodeId.backup).event.isInstanceOf[ClusterFailedOver])
        val eventId = primaryDirector.eventWatch.lastAddedEventId

        // FIXME Delay until AgentOrderKeeper does not persist anything,
        // because it cannot be terminated while persisting and we would stick in a deadlock.
        sleep(1.s)

        /// Restart the primary ///

        sys.props(testHeartbeatLossPropertyKey) = "false"
        sys.props(testAckLossPropertyKey) = "false"

        primaryDirector
          .terminate(clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))
          .await(99.s)

        primaryDirector = startDirector(primaryDirectorEnvAllocated)
        primaryDirector.eventWatch.await[ClusterCoupled](after = eventId)

        /// Now, nodeToLossNotConfirmedProblem must be empty ///

        assert(controller.controllerState().keyTo(AgentRefState)(agentPath)
          .nodeToLossNotConfirmedProblem.isEmpty)

        // FIXME Delay, otherwise SubagentEventListener.observeEvents may not be stoppable
        sleep(1.s)
        (primaryDirector.terminate() *> backupDirector.terminate())
          .await(99.s)
    }

object AgentClusterNodeLossRestartTest:
  private val logger = Logger[this.type]

  private val clusterTiming = ClusterTiming(heartbeat = 500.ms, heartbeatTimeout = 500.ms)

  private val subagentIds = Seq(
    SubagentId("SUBAGENT-0"),
    SubagentId("SUBAGENT-1"))

  private val agentPath = AgentPath("AGENT")
  private val agentRef = AgentRef(agentPath, subagentIds)

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
