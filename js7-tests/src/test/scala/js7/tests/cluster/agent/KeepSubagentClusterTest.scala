package js7.tests.cluster.agent

import cats.effect.{IO, ResourceIO}
import js7.agent.data.commands.AgentCommand
import js7.agent.{RunningAgent, TestAgent}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentRefStateEvent.AgentMirroredEvent
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver}
import js7.data.cluster.ClusterState
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.agent.KeepSubagentClusterTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.util.control.NonFatal

final class KeepSubagentClusterTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    js7.auth.agents.$agentPath = "${agentPath.toString}-PASSWORD" # TODO For directorEnvResource 👇
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.journal.cluster.heartbeat = 0.5s
    js7.journal.cluster.heartbeat-timeout = 0.5s
    """

  protected override val agentPaths = Seq()

  private lazy val primarySubagentItem =
    SubagentItem(primarySubagentId, agentPath, findFreeLocalUri())
  private lazy val backupSubagentItem =
    SubagentItem(backupSubagentId, agentPath, findFreeLocalUri())
  private lazy val agentRef = AgentRef(agentPath, Seq(primarySubagentId, backupSubagentId))

  protected def items = Nil

  "Job started at backup Subagent while failing-over" in:
    val primaryDirectorResource: ResourceIO[RunningAgent] =
      directoryProvider
        .directorEnvResource(
          primarySubagentItem, otherSubagentIds = Seq(backupSubagentId))
        .flatMap(_.directorResource)

    val backupDirectorResource: ResourceIO[RunningAgent] =
      directoryProvider
        .directorEnvResource(
          backupSubagentItem, otherSubagentIds = Seq(primarySubagentId), isClusterBackup = true)
        .flatMap(_.directorResource)

    TestAgent(primaryDirectorResource.toAllocated.await(99.s)).useSync(99.s) { primaryDirector =>
      TestAgent(backupDirectorResource.toAllocated.await(99.s)).useSync(99.s) { backupDirector =>
        try
          var eventId = eventWatch.lastAddedEventId
          updateItems(
            workflow,
            primarySubagentItem,
            backupSubagentItem,
            agentRef.copy(
              itemRevision = None))

          eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
            && ke.event.key == agentPath, after = eventId)
          eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
            && ke.event.key == backupSubagentItem.id, after = eventId)

          primaryDirector.eventWatch.await[ClusterCoupled]()

          ASemaphoreJob.continue()
          assert(controller.runOrder(FreshOrder(OrderId("🔹"), workflow.path))
            .last.value == OrderFinished())

          def agentClusterState() = controller.controllerState().keyTo(AgentRefState)(agentPath)
            .clusterState
          assert(agentClusterState().isInstanceOf[ClusterState.Coupled])

          val failOverOrderId = OrderId("🔺")
          controller.api.addOrder(FreshOrder(failOverOrderId, workflow.path)).await(99.s).orThrow
          controller.eventWatch.await[OrderProcessingStarted](_.key == failOverOrderId)
          controller.eventWatch.await[OrderStdoutWritten](_.key == failOverOrderId)

          // Kill Agent roughly
          primaryDirector.terminate(
            processSignal = Some(SIGKILL),
            clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))
            .await(99.s)

          backupDirector.untilReady.await(99.s)
          eventId = controller.eventWatch
            .await[AgentMirroredEvent](_.event.keyedEvent.event.isInstanceOf[ClusterFailedOver])
            .head.eventId
          ASemaphoreJob.continue()

          backupDirector.eventWatch.await[ClusterFailedOver]()
          controller.eventWatch.await[OrderTerminated](_.key == failOverOrderId, after = eventId)
          assert(agentClusterState().isInstanceOf[ClusterState.FailedOver])

          controller.eventWatch.await[OrderFinished](_.key == failOverOrderId)
        catch
          case NonFatal(t) =>
            logger.error(t.toStringWithCauses, t)
            throw t
      }
    }


object KeepSubagentClusterTest:

  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val primarySubagentId = toLocalSubagentId(agentPath)
  private val backupSubagentId = SubagentId("Backup-SUBAGENT")

  private val workflow = Workflow(
    WorkflowPath("MY-WORKFLOW"),
    Seq(
      ASemaphoreJob.execute(
        agentPath,
        subagentBundleId = Some(StringConstant(backupSubagentId.string)))))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
