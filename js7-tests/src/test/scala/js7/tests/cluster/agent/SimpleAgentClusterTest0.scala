package js7.tests.cluster.agent

import cats.effect.IO
import js7.agent.{RunningAgent, TestAgent}
import js7.base.log.Logger
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentRefStateEvent.AgentMirroredEvent
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.cluster.{ClusterState, Confirmer}
import js7.data.controller.ControllerCommand.ConfirmClusterNodeLoss
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemRevision
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentId, SubagentItem}
import js7.data.value.expression.Expression.{NumericConstant, StringConstant}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.cluster.agent.SimpleAgentClusterTest0.*
import js7.tests.cluster.controller.ControllerClusterTester
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectorEnv
import js7.tests.testenv.ProgramEnvTester.assertEqualJournalFiles
import scala.util.control.NonFatal

abstract sealed class SimpleAgentClusterTest0 extends ControllerClusterTester:

  protected override val agentPaths = Nil

  private lazy val subagentItems = Seq(
    SubagentItem(SubagentId("SUBAGENT-0"), agentPath, findFreeLocalUri()),
    SubagentItem(SubagentId("SUBAGENT-1"), agentPath, findFreeLocalUri()))

  override protected def items =
    Seq(TestWorkflow, workflow, agentRef, subagentBundle) ++ subagentItems

  protected final def runMyTest(requireFailoverConfirmation: Boolean): Unit =
    withControllerAndBackupWithoutAgents() { (primary, backup, _) =>

      def allocateDirector(director: SubagentItem, otherDirectorId: SubagentId,
        isBackup: Boolean = false)
      : Allocated[IO, (DirectorEnv, RunningAgent)] =
        primary
          .directorEnvResource(director, otherSubagentIds = Seq(otherDirectorId),
            isClusterBackup = isBackup)
          .flatMap(env => env.directorResource.map(env -> _))
          .toAllocated
          .await(99.s)

      val primaryDirectorAllocated: Allocated[IO, (DirectorEnv, RunningAgent)] =
        allocateDirector(subagentItems(0), otherDirectorId = subagentItems(1).id)

      val backupDirectorAllocated: Allocated[IO, (DirectorEnv, RunningAgent)] =
        allocateDirector(subagentItems(1), otherDirectorId = subagentItems(0).id, isBackup = true)

      TestAgent(primaryDirectorAllocated.map(_._2)).useSync(99.s) { primaryDirector =>
        val primaryDirectorEnv = primaryDirectorAllocated.allocatedThing._1
        TestAgent(backupDirectorAllocated.map(_._2)).useSync(99.s) { backupDirector =>
          val backupDirectorEnv = backupDirectorAllocated.allocatedThing._1
          try
            runControllers(primary, backup) { (primaryController, _) =>
              import primaryController.eventWatch.await
              val failOverOrderId = OrderId("🔺")
              primaryDirector.eventWatch.await[ClusterNodesAppointed]()
              primaryDirector.eventWatch.await[ClusterWatchRegistered]()
              primaryDirector.eventWatch.await[ClusterCoupled]()

              for delegateId <- agentPath +: subagentIds do
                primaryController.eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
                  && ke.event.key == delegateId)

              assert(primaryController.controllerState().itemToAgentToAttachedState == Map(
                agentPath      -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
                subagentIds(0) -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
                subagentIds(1) -> Map(agentPath -> Attached(Some(ItemRevision(0))))))

              if requireFailoverConfirmation then
                val agentRef = primaryController.controllerState().keyToItem(AgentRef)(agentPath)
                primaryController.api.updateUnsignedSimpleItems:
                  Seq:
                    agentRef.copy(requireFailoverConfirmation = true, itemRevision = None)
                .await(99.s).orThrow

              val stampedSeq = primaryController.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
              assert(stampedSeq.last.value == OrderFinished())

              def agentClusterState() = primaryController.controllerState().keyTo(AgentRefState)(agentPath)
                .clusterState
              assert(agentClusterState().isInstanceOf[ClusterState.Coupled])

              assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)
              assertEqualJournalFiles(primaryDirectorEnv, backupDirectorEnv, n = 1)

              primaryController.api.addOrder:
                FreshOrder(failOverOrderId, workflow.path, scheduledFor = Some(Timestamp.now + 1.s))
              .await(99.s).orThrow
              val forceOrderProcessingStarted = await[OrderProcessingStarted](_.key == failOverOrderId)
                .last.value.event
              assert(forceOrderProcessingStarted.subagentId contains subagentIds(0))

              primaryDirector.kill.await(99.s)

              if requireFailoverConfirmation then
                val clusterWatch = primaryController.clusterWatchFor(agentPath).orThrow
                awaitAndAssert:
                  clusterWatch.clusterNodeLossEventToBeConfirmed(primaryId)
                    .exists(_.isInstanceOf[ClusterFailedOver])
                primaryController.execCmd:
                  ConfirmClusterNodeLoss(agentPath, NodeId.primary, Confirmer("SimpleAgentClusterTest0"))

              val eventId = primaryController.eventWatch
                .await[AgentMirroredEvent](_.event.keyedEvent.event.isInstanceOf[ClusterFailedOver])
                .head.eventId
              ASemaphoreJob.continue()

              val secondProcessingStarted = primaryController.eventWatch
                .await[OrderProcessingStarted](_.key == failOverOrderId, after = eventId)
                .last.value.event
              assert(secondProcessingStarted.subagentId contains subagentIds(1))
              primaryController.eventWatch.await[OrderDetachable](_.key == failOverOrderId,
                after = eventId)
              assert(agentClusterState().isInstanceOf[ClusterState.FailedOver])

              primaryController.eventWatch.await[OrderFinished](_.key == failOverOrderId)

              ASemaphoreJob.continue()
              val bOrderId = OrderId("🔸")
              primaryController.runOrder(FreshOrder(bOrderId, workflow.path))
              assert(agentClusterState().isInstanceOf[ClusterState.FailedOver])
            }
          catch
            case NonFatal(t) =>
              logger.error(t.toStringWithCauses, t)
              throw t
        }
      }
    }
  end runMyTest


object SimpleAgentClusterTest0:
  private val logger = Logger[this.type]

  private val subagentIds = Seq(
    SubagentId("SUBAGENT-0"),
    SubagentId("SUBAGENT-1"))

  private val agentPath = AgentPath("AGENT")
  private val agentRef = AgentRef(agentPath, subagentIds)
  private val subagentBundle = SubagentBundle(
    SubagentBundleId("SUBAGENT-BUNDLE"),
    Map(
      subagentIds(0) -> NumericConstant(2),
      subagentIds(1) -> NumericConstant(1))
  )

  private val workflow = Workflow(
    WorkflowPath("MY-WORKFLOW"),
    Seq(ASemaphoreJob.execute(
      AgentPath("AGENT"),
      subagentBundleId = Some(StringConstant(subagentBundle.id.string)))))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]


final class SimpleAgentClusterTest extends SimpleAgentClusterTest0:

  "Cluster replicates journal files properly" in:
    runMyTest(requireFailoverConfirmation = false)


final class ExternallyConfirmingFailoverAgentClusterTest extends SimpleAgentClusterTest0:

  "Cluster replicates journal files properly" in:
    runMyTest(requireFailoverConfirmation = true)
