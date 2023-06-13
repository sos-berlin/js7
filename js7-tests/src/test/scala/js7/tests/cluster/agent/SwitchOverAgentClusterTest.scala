package js7.tests.cluster.agent

import cats.effect.Resource
import js7.agent.{RunningAgent, TestAgent}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ProgramTermination
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterSwitchedOver}
import js7.data.controller.ControllerCommand.ClusterSwitchOver
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessingStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.Problems.SubagentHasStillOrdersProblem
import js7.data.subagent.SubagentItemStateEvent.SubagentDedicated
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.journal.files.JournalFiles.JournalMetaOps
import js7.tests.cluster.agent.SwitchOverAgentClusterTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest, DirectorEnv}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.util.control.NonFatal

final class SwitchOverAgentClusterTest extends OurTestSuite with ControllerAgentForScalaTest
with BlockingItemUpdater
{
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

  "SwitchOver to backup" in {
    /** Returned `TestAgent` releases `DirectorEnv`, too. */
    def toTestAgent(envResource: Resource[Task, DirectorEnv]): (DirectorEnv, TestAgent) = {
      val resource: Resource[Task, (DirectorEnv, RunningAgent)] =
        for {
          env <- envResource
          program <- env.programResource
        } yield env -> program
      val allocated = resource.toAllocated.await(99.s)
      allocated.allocatedThing._1 -> TestAgent(allocated.map(_._2)/*, Some(SIGTERM) ???*/)
    }

    val (primaryEnv, primaryDirector) = toTestAgent(
      directoryProvider.directorEnvResource(
        primarySubagentItem,
        moreSubagentIds = Seq(backupSubagentId)))

    val (backupEnv, backupDirector) = toTestAgent(
      directoryProvider.directorEnvResource(
        backupSubagentItem,
        moreSubagentIds = Seq(primarySubagentId),
        isClusterBackup = true))

    backupDirector.useSync(99.s) { _ =>
      primaryDirector.useSync(99.s) { _ =>
        try {
          updateItems(primarySubagentItem, backupSubagentItem, agentRef, workflow)

          primaryDirector.eventWatch.await[SubagentDedicated](_.key == primarySubagentId)
          primaryDirector.eventWatch.await[SubagentDedicated](_.key == backupSubagentId)

          primaryDirector.eventWatch.await[ClusterCoupled]()
          assert(primaryEnv.journalLocation.listJournalFiles.nonEmpty)
          assert(backupEnv.journalLocation.listJournalFiles.nonEmpty)

          val orderId = OrderId("🔹")
          controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
          val processingStarted = controller.eventWatch
            .await[OrderProcessingStarted](_.key == orderId).last.value.event
          assert(processingStarted.subagentId contains primarySubagentId)
          controller.eventWatch.await[OrderStdoutWritten](_.key == orderId)

          assert(controller.api.executeCommand(ClusterSwitchOver(Some(agentPath))).await(99.s) ==
            Left(SubagentHasStillOrdersProblem(primarySubagentId)))

          ASemaphoreJob.continue()
          controller.eventWatch.await[OrderTerminated](_.key == orderId)

          controller.api.executeCommand(ClusterSwitchOver(Some(agentPath))).await(99.s).orThrow
          backupDirector.eventWatch.await[ClusterSwitchedOver]()
          val termination = primaryDirector.untilTerminated.await(99.s)
          assert(termination == ProgramTermination(restart = true))

          locally {
            val orderId = OrderId("🔶")
            controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
            val processingStarted = controller.eventWatch
              .await[OrderProcessingStarted](_.key == orderId).last.value.event
            assert(processingStarted.subagentId contains backupSubagentId)
            controller.eventWatch.await[OrderStdoutWritten](_.key == orderId)

            ASemaphoreJob.continue()
            controller.eventWatch.await[OrderFinished](_.key == orderId)
          }
        } catch {
          case NonFatal(t) =>
            logger.error(t.toStringWithCauses, t)
            throw t.appendCurrentStackTrace
        }
      }
    }
  }
}

object SwitchOverAgentClusterTest {
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val primarySubagentId = toLocalSubagentId(agentPath)
  private val backupSubagentId = SubagentId("Backup-SUBAGENT")

  private val workflow = Workflow(
    WorkflowPath("MY-WORKFLOW"),
    Seq(
      ASemaphoreJob.execute(agentPath)))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
}
