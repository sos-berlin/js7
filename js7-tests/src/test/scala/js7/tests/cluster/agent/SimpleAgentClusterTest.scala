package js7.tests.cluster.agent

import js7.agent.{RunningAgent, TestAgent}
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentRefStateEvent.AgentMirroredEvent
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.cluster.ClusterState
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemRevision
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentSelection, SubagentSelectionId}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.agent.SimpleAgentClusterTest.*
import js7.tests.cluster.controller.ControllerClusterTester
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectorEnv
import js7.tests.testenv.ProgramEnvTester.assertEqualJournalFiles
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.util.control.NonFatal

final class SimpleAgentClusterTest extends ControllerClusterTester
{
  protected override val agentPaths = Nil

  private lazy val subagentItems = Seq(
    SubagentItem(SubagentId("SUBAGENT-0"), agentPath, findFreeLocalUri()),
    SubagentItem(SubagentId("SUBAGENT-1"), agentPath, findFreeLocalUri()))

  override protected def items =
    Seq(TestWorkflow, workflow, agentRef, subagentSelection) ++ subagentItems

  "Cluster replicates journal files properly" in {
    withControllerAndBackupWithoutAgents() { (primary, backup, _) =>


      val primaryDirectorAllocated: Allocated[Task, (DirectorEnv, RunningAgent)] =
        primary
          .directorEnvResource(
            subagentItems(0),
            otherSubagentIds = Seq(subagentItems(1).id))
          .flatMap(env => env.directorResource.map(env -> _))
          .toAllocated
          .await(99.s)

      val backupDirectorAllocated: Allocated[Task, (DirectorEnv, RunningAgent)] =
        primary
          .directorEnvResource(
            subagentItems(1),
            otherSubagentIds = Seq(subagentItems(0).id),
            isClusterBackup = true)
          .flatMap(env => env.directorResource.map(env -> _))
          .toAllocated
          .await(99.s)

      TestAgent(primaryDirectorAllocated.map(_._2)).useSync(99.s) { primaryDirector =>
        val primaryDirectorEnv = primaryDirectorAllocated.allocatedThing._1
        TestAgent(backupDirectorAllocated.map(_._2)).useSync(99.s) { backupDirector =>
          val backupDirectorEnv = backupDirectorAllocated.allocatedThing._1
          try {
            runControllers(primary, backup) { (primaryController, _) =>
              import primaryController.eventWatch.await
              val failOverOrderId = OrderId("🔺")
              primaryDirector.eventWatch.await[ClusterNodesAppointed]()
              primaryDirector.eventWatch.await[ClusterWatchRegistered]()
              primaryDirector.eventWatch.await[ClusterCoupled]()

              for (delegateId <- agentPath +: subagentIds) {
                primaryController.eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
                  && ke.event.key == delegateId)
              }

              assert(primaryController.controllerState().itemToAgentToAttachedState == Map(
                agentPath      -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
                subagentIds(0) -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
                subagentIds(1) -> Map(agentPath -> Attached(Some(ItemRevision(0))))))

              val stampedSeq = primaryController.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
              assert(stampedSeq.last.value == OrderFinished())

              def agentClusterState() = primaryController.controllerState().keyTo(AgentRefState)(agentPath)
                .clusterState
              assert(agentClusterState().isInstanceOf[ClusterState.Coupled])

              assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)
              assertEqualJournalFiles(primaryDirectorEnv, backupDirectorEnv, n = 1)

              primaryController.api.addOrder(FreshOrder(failOverOrderId, workflow.path)).await(99.s).orThrow
              val forceOrderProcessingStarted = await[OrderProcessingStarted](_.key == failOverOrderId)
                .last.value.event
              assert(forceOrderProcessingStarted.subagentId contains subagentIds(0))

              primaryDirector.killForFailOver.await(99.s)

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
          } catch {
            case NonFatal(t) =>
              logger.error(t.toStringWithCauses, t)
              throw t
          }
        }
      }
    }
  }
}

object SimpleAgentClusterTest {
  private val logger = Logger[this.type]

  private val subagentIds = Seq(
    SubagentId("SUBAGENT-0"),
    SubagentId("SUBAGENT-1"))

  private val agentPath = AgentPath("AGENT")
  private val agentRef = AgentRef(agentPath, subagentIds)
  private val subagentSelection = SubagentSelection(
    SubagentSelectionId("SUBAGENT-SELECTION"),
    Map(
      subagentIds(0) -> 2,
      subagentIds(1) -> 1)
  )

  private val workflow = Workflow(
    WorkflowPath("MY-WORKFLOW"),
    Seq(ASemaphoreJob.execute(
      AgentPath("AGENT"),
      subagentSelectionId = Some(StringConstant(subagentSelection.id.string)))))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
}
