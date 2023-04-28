package js7.tests.controller.cluster

import cats.effect.Resource
import cats.syntax.all.*
import js7.agent.data.commands.AgentCommand
import js7.agent.{ConvertibleSubagent, TestAgent}
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Allocated
import js7.base.utils.CatsBlocking.BlockingTaskResource
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichThrowable}
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentRefStateEvent.AgentMirroredEvent
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.ClusterEvent.{ClusterCoupled, ClusterFailedOver, ClusterNodesAppointed, ClusterWatchRegistered}
import js7.data.cluster.ClusterState
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.controller.cluster.SimpleAgentClusterTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import js7.tests.testenv.SubagentEnv
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.util.control.NonFatal

final class SimpleAgentClusterTest extends ControllerClusterTester
{
  protected override val agentPaths = Nil

  private lazy val subagentItems = Seq(
    SubagentItem(SubagentId("SUBAGENT-0"), agentPath, Uri("http://127.0.0.1:" + findFreeTcpPort())),
    SubagentItem(SubagentId("SUBAGENT-1"), agentPath, Uri("http://127.0.0.1:" + findFreeTcpPort())))

  override protected def items = Seq(TestWorkflow, workflow, agentRef) ++ subagentItems

  "Cluster replicates journal files properly" in {
    withControllerAndBackupWithoutAgents() { (primary, backup, _) =>
      val subagentResources: Resource[Task, Seq[(SubagentEnv, ConvertibleSubagent)]] =
        Seq(
          primary/*any DirectoryProvider*/.subagentEnvResource(subagentItems(0)),
          primary/*any DirectoryProvider*/.subagentEnvResource(subagentItems(1),
            isClusterBackup = true)
        ).sequence
          .flatMap(_.traverse(env => env.convertibleSubagentResource.map(env -> _)))

      subagentResources.blockingUse(99.s)(envsAndConvertibleSubagents => try {
        val (subagentEnvs, convertibleSubagents) = envsAndConvertibleSubagents.unzip
        runControllers(primary, backup) { (primaryController, _) =>
          import primaryController.eventWatch.await
          val failOverOrderId = OrderId("🔺")
          val agent = convertibleSubagents(0).untilDirectorStarted.await(99.s)
          agent.eventWatch.await[ClusterNodesAppointed]()
          agent.eventWatch.await[ClusterWatchRegistered]()
          agent.eventWatch.await[ClusterCoupled]()

          val stampedSeq = primaryController.runOrder(FreshOrder(OrderId("🔹"), TestWorkflow.path))
          assert(stampedSeq.last.value == OrderFinished())

          def agentClusterState() = primaryController.controllerState().keyTo(AgentRefState)(agentPath)
            .clusterState
          assert(agentClusterState().isInstanceOf[ClusterState.Coupled])

          assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)
          assertEqualJournalFiles(subagentEnvs(0), subagentEnvs(1), n = 1)

          primaryController.api.addOrder(FreshOrder(failOverOrderId, workflow.path)).await(99.s).orThrow
          await[OrderProcessingStarted](_.key == failOverOrderId)
          //TODO: await[OrderStdoutWritten](_.key == failOverOrderId)

          // Kill Agent roughly — TODO Proper fast Agent termination desired
          //agent.journal.await(99.s).journalActor ! PoisonPill
          //Try(agent.untilTerminated.await(99.s))
          new TestAgent(new Allocated(agent, Task.unit))
            .terminate(
              processSignal = Some(SIGKILL),
              clusterAction = Some(AgentCommand.ShutDown.ClusterAction.Failover))
            .await(99.s)

          val backupDirector = convertibleSubagents(1).untilDirectorStarted.await(99.s)
          val eventId = primaryController.eventWatch
            .await[AgentMirroredEvent](_.event.keyedEvent.event.isInstanceOf[ClusterFailedOver])
            .head.eventId
          ASemaphoreJob.continue()

          primaryController.eventWatch.await[OrderProcessingStarted](_.key == failOverOrderId, after = eventId)
          primaryController.eventWatch.await[OrderDetachable](_.key == failOverOrderId, after = eventId)
          assert(agentClusterState().isInstanceOf[ClusterState.FailedOver])

          primaryController.eventWatch.await[OrderFinished](_.key == failOverOrderId)

          ASemaphoreJob.continue()
          val bOrderId = OrderId("🔸")
          primaryController.runOrder(FreshOrder(bOrderId, workflow.path))
          assert(agentClusterState().isInstanceOf[ClusterState.FailedOver])
        }
      } catch {
        case NonFatal(t) =>
          // TODO Move this code to blockingUse
          logger.error(t.toStringWithCauses, t)
          try envsAndConvertibleSubagents.parTraverse(_._2.stop).await(99.s)
          catch {
            case NonFatal(t2) => t.addSuppressed(t2)
          }
          throw t
      })
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

  private val workflow = Workflow(
    WorkflowPath("MY-WORKFLOW"),
    Seq(ASemaphoreJob.execute(AgentPath("AGENT"))))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]
}
