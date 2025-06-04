package js7.tests.cluster.agent

import cats.effect.IO
import js7.agent.data.commands.AgentCommand
import js7.agent.{RunningAgent, TestAgent}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.AgentRefStateEvent.AgentMirroredEvent
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.cluster.ClusterState
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemRevision
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.agent.BecomeAgentClusterTest.*
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class BecomeAgentClusterTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.journal.cluster.heartbeat = 0.5s
    js7.journal.cluster.heartbeat-timeout = 0.5s

    js7.auth.subagents.${backupSubagentItem.id.string} = "ACTIVE DIRECTOR'S PASSWORD"
    js7.auth.users.${backupSubagentItem.id.string} {
      permissions: [ AgentDirector ]
      password: "plain:BACKUP DIRECTOR'S PASSWORD"
    }
    """

  override val agentPaths = Seq(agentPath)

  private lazy val backupSubagentItem = SubagentItem(
    backupSubagentId, agentPath, findFreeLocalUri())

  protected def items = Seq(workflow)

  "Run an Order without cluster" in:
    val stampedEvents = controller.runOrder(FreshOrder(OrderId("SIMPLE"), workflow.path))
    assert(stampedEvents.last.value.isInstanceOf[OrderFinished])

  "Add a backup Subagent" in:
    val subagentAllocated: Allocated[IO, RunningAgent] =
      directoryProvider
        .directorEnvResource(backupSubagentItem, isClusterBackup = true,
          extraConfig = config"""
            js7.auth.subagents.${primarySubagentId.string} = "BACKUP DIRECTOR'S PASSWORD"
            js7.auth.users.${primarySubagentId.string} {
              permissions: [ AgentDirector ]
              password: "plain:ACTIVE DIRECTOR'S PASSWORD"
            }""")
        .flatMap(_.directorResource)
        .toAllocated
        .await(99.s)

    TestAgent(subagentAllocated).useSync(99.s): backupDirector =>
      val agentRef =
        val a = controllerState.keyToItem(AgentRef)(agentPath)
        a.copy(
          directors = a.directors :+ backupSubagentId,
          itemRevision = None)

      val semaWorkflow = Workflow(
        WorkflowPath("SEMAPHORE-WORKFLOW"),
        Seq:
          ASemaphoreJob.execute(agentPath, isNotRestartable = true,
            subagentBundleId = Some(StringConstant(backupSubagentId.string))))

      val eventId = eventWatch.resetLastWatchedEventId()

      updateItems(agentRef, backupSubagentItem, semaWorkflow)
      assert(controller.runOrder(FreshOrder(OrderId("🟦"), workflow.path))
        .last.value == OrderFinished())

      eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
        && ke.event.key == agentPath, after = eventId)
      eventWatch.await[ItemAttached](ke => ke.event.delegateId == agentPath
        && ke.event.key == backupSubagentItem.id, after = eventId)

      assert(controllerState.itemToAgentToAttachedState == Map(
        // 👇ItemRevision(1) because AgentRef has been changed
        agentPath -> Map(agentPath -> Attached(Some(ItemRevision(1)))),
        primarySubagentId -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
        // 👇backupSubagentItem is attached to Agent now
        backupSubagentItem.id -> Map(agentPath -> Attached(Some(ItemRevision(0)))),
        workflow.id -> Map(agentPath -> Attached(None))))

      agent.eventWatch.await[ClusterNodesAppointed]()
      agent.eventWatch.await[ClusterWatchRegistered]()
      agent.eventWatch.await[ClusterCoupled]()

      assert:
        controller.runOrder:
          FreshOrder(OrderId("🔹"), workflow.path, deleteWhenTerminated = true)
        .exists(_.value == OrderFinished())

      assert(controllerState.keyTo(AgentRefState)(agentPath).clusterState
        .isInstanceOf[ClusterState.Coupled])

      val failoverOrderId = OrderId("🔺")
      controller.api.addOrder:
        FreshOrder(failoverOrderId, semaWorkflow.path, deleteWhenTerminated = true)
      .await(99.s).orThrow
      eventWatch.awaitNextKey[OrderProcessingStarted](failoverOrderId)
      eventWatch.awaitNextKey[OrderStdoutWritten](failoverOrderId)

      // Kill Agent roughly //
      agent.terminate(
          clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))
        .await(99.s)

      // Failed over //
      eventWatch
        .awaitNext[AgentMirroredEvent](_.event.keyedEvent.event.isInstanceOf[ClusterFailedOver])
        .head.eventId

      ASemaphoreJob.continue()
      eventWatch.awaitNextKey[OrderProcessed](failoverOrderId)
      eventWatch.awaitNextKey[OrderFinished](failoverOrderId)


object BecomeAgentClusterTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val primarySubagentId = toLocalSubagentId(agentPath)
  private val backupSubagentId = SubagentId("Backup-SUBAGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Seq(
      EmptyJob.execute(agentPath)))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
