package js7.tests.cluster.agent

import cats.effect.Resource
import js7.agent.ConvertibleSubagent
import js7.agent.data.commands.AgentCommand
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentRefStateEvent.AgentMirroredEvent
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.cluster.ClusterState
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemRevision
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.agent.BecomeAgentClusterTest.*
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest, SubagentEnv}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.util.control.NonFatal

final class BecomeAgentClusterTest extends OurTestSuite with ControllerAgentForScalaTest
with BlockingItemUpdater
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.journal.cluster.heartbeat = 0.5s
    js7.journal.cluster.heartbeat-timeout = 0.5s
    """

  protected override val agentPaths = Seq(agentPath)

  private lazy val backupSubagentItem = SubagentItem(
    SubagentId("Backup-SUBAGENT"), agentPath, Uri("http://127.0.0.1:" + findFreeTcpPort()))

  override protected def items = Seq(workflow)

  "Run an Order without cluster" in {
    val stampedEvents = controller.runOrder(FreshOrder(OrderId("SIMPLE"), workflow.path))
    assert(stampedEvents.last.value.isInstanceOf[OrderFinished])
  }

  "Add a backup Subagent" in {
    val subagentResource: Resource[Task, (SubagentEnv, ConvertibleSubagent)] =
      directoryProvider
        .subagentEnvResource(backupSubagentItem, isClusterBackup = true)
        .flatMap(env => env.convertibleSubagentResource.map(env -> _))

    subagentResource.blockingUse(99.s) { case (subagentEnv, convertibleSubagent) =>
      try {
        assert(controller.runOrder(FreshOrder(OrderId("🟦"), workflow.path))
          .last.value == OrderFinished())

        assert(controllerState.itemToAgentToAttachedState == Map(
          agentPath -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
          toLocalSubagentId(agentPath) -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
          workflow.id -> Map(agentPath -> Attached(None))))

        var eventId = eventWatch.lastAddedEventId
        val agentRef = controllerState.keyToItem(AgentRef)(agentPath)
        updateItems(
          backupSubagentItem,
          agentRef.copy(
            directors = agentRef.directors :+ backupSubagentItem.id,
            itemRevision = None))

        eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
          && ke.event.key == agentPath, after = eventId)
        eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
          && ke.event.key == backupSubagentItem.id, after = eventId)

        assert(controllerState.itemToAgentToAttachedState == Map(
          // 👇ItemRevision(1) because AgentRef has been changed
          agentPath -> Map(agentPath -> Attached(Some(ItemRevision(1)))),
          toLocalSubagentId(agentPath) -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
          // 👇backupSubagentItem is attached to Agent now
          backupSubagentItem.id -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
          workflow.id -> Map(agentPath -> Attached(None))))

        agent.eventWatch.await[ClusterNodesAppointed]()
        agent.eventWatch.await[ClusterWatchRegistered]()
        agent.eventWatch.await[ClusterCoupled]()

        assert(controller.runOrder(FreshOrder(OrderId("🔹"), workflow.path))
          .last.value == OrderFinished())

        def agentClusterState() = controller.controllerState().keyTo(AgentRefState)(agentPath)
          .clusterState
        assert(agentClusterState().isInstanceOf[ClusterState.Coupled])

        //assertEqualJournalFiles(directoryProvider.subagentEnv(x), subagentEnv, n = 1)

        val failOverOrderId = OrderId("🔺")
        controller.api.addOrder(FreshOrder(failOverOrderId, workflow.path)).await(99.s).orThrow
        //await[OrderProcessingStarted](_.key == failOverOrderId)
        ////TODO: await[OrderStdoutWritten](_.key == failOverOrderId)

        // Kill Agent roughly
        agent.terminate(
          processSignal = Some(SIGKILL),
          clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))
          .await(99.s)

        val backupDirector = convertibleSubagent.untilDirectorStarted.await(99.s)
        eventId = controller.eventWatch
          .await[AgentMirroredEvent](_.event.keyedEvent.event.isInstanceOf[ClusterFailedOver])
          .head.eventId
        //ASemaphoreJob.continue()
        //
        //controller.eventWatch.await[OrderProcessingStarted](_.key == failOverOrderId, after = eventId)
        //controller.eventWatch.await[OrderDetachable](_.key == failOverOrderId, after = eventId)
        //assert(agentClusterState().isInstanceOf[ClusterState.FailedOver])
        //
        //controller.eventWatch.await[OrderFinished](_.key == failOverOrderId)
        //
        //ASemaphoreJob.continue()
        //val bOrderId = OrderId("🔸")
        //controller.runOrder(FreshOrder(bOrderId, workflow.path))
        //assert(agentClusterState().isInstanceOf[ClusterState.FailedOver])
      } catch {
        case NonFatal(t) =>
          // TODO Move this code to blockingUse
          logger.error(t.toStringWithCauses, t)
          try convertibleSubagent.stop.await(99.s)
          catch {
            case NonFatal(t2) => t.addSuppressed(t2)
          }
          throw t
      }
    }
  }
}

object BecomeAgentClusterTest {
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("MY-WORKFLOW") ~ "INITIAL",
    Seq(
      EmptyJob.execute(agentPath)))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
}
