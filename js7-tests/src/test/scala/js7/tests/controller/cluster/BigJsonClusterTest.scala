package js7.tests.controller.cluster

import js7.base.auth.Admission
import js7.base.configutils.Configs.RichConfig
import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.common.configuration.JobSchedulerConfiguration
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.agent.AgentPath
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.job.InternalExecutable
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderFinished, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.proxy.ControllerApi
import js7.tests.controller.cluster.BigJsonClusterTest._
import js7.tests.testenv.ControllerClusterForScalaTest
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class BigJsonClusterTest extends AnyFreeSpec with ControllerClusterForScalaTest
{
  protected val items = Seq(workflow)
  override protected val clusterTiming = ClusterTiming(1.s, 10.s)

  "Cluster replicates big JSON" in {
    withControllerAndBackup() { (primary, backup, _) =>
      val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
      val primaryController = primary.startController(httpPort = Some(primaryControllerPort))
        .await(99.s)
      import primaryController.eventWatch
      eventWatch.await[ClusterEvent.ClusterCoupled]()

      val admissions = Seq(
        Admission(primaryController.localUri, Some(userAndPassword)),
        Admission(backupController.localUri, Some(userAndPassword)))
      val controllerApi = new ControllerApi(
        admissionsToApiResources(admissions)(primaryController.actorSystem))

      val orderId = OrderId("BIG-ORDER")
      controllerApi
        .addOrders(Observable(FreshOrder(orderId, workflow.path, Map(
          "ARG" -> StringValue(bigString)))))
        .await(99.s).orThrow
      val event = eventWatch.await[OrderTerminated](_.key == orderId)
      assert(event.head.value.event == OrderFinished)

      val controllerState = controllerApi.controllerState.await(99.s).orThrow
      assert(controllerState.clusterState.asInstanceOf[ClusterState.Coupled].setting.activeId ==
        NodeId("Primary"))

      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)

      controllerApi.stop await 99.s
      primaryController.terminate() await 99.s
      backupController.terminate() await 99.s
    }
  }
}

object BigJsonClusterTest
{
  private val agentPath = AgentPath("AGENT")
  private def bigString = "+" *
    (9_000_000 max
      JobSchedulerConfiguration.defaultConfig.memorySizeAsInt("js7.web.chunk-size").orThrow)

  private val workflow = Workflow(WorkflowPath("BIG-JSON") ~ "INITIAL",
    Seq.fill(2)(
      Execute(WorkflowJob(
        agentPath,
        InternalExecutable(
          classOf[TestJob].getName,
          arguments = Map(
            "BIG" -> StringConstant(bigString)))))))

  final class TestJob extends InternalJob
  {
    private val orderProcess = OrderProcess.succeeded(Map(
      "RESULT" -> StringValue(bigString)))

    def toOrderProcess(step: Step) = orderProcess
  }
}
