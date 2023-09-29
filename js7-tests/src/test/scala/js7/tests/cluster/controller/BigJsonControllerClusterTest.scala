package js7.tests.cluster.controller

import js7.base.auth.Admission
import js7.base.configutils.Configs.RichConfig
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.Nel
import js7.common.configuration.Js7Configuration
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResource
import js7.data.agent.AgentPath
import js7.data.cluster.{ClusterEvent, ClusterState, ClusterTiming}
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderFinished, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.proxy.ControllerApi
import js7.tests.cluster.controller.BigJsonControllerClusterTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import js7.tests.testenv.ProgramEnvTester.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable

final class BigJsonControllerClusterTest extends OurTestSuite with ControllerClusterForScalaTest:
  private val bigString = BigJsonControllerClusterTest.bigString // Allocate one

  private val workflow = Workflow(WorkflowPath("BIG-JSON") ~ "INITIAL",
    Seq.fill(2)(
      TestJob.execute(agentPath, arguments = Map(
        "BIG" -> StringConstant(bigString)))))

  protected val items = Seq(workflow)
  override protected val clusterTiming = ClusterTiming(1.s, 10.s)

  "Cluster replicates big JSON" in:
    runControllerAndBackup() { (primary, primaryController, _, backup, backupController, _, _) =>
      import primaryController.eventWatch
      eventWatch.await[ClusterEvent.ClusterCoupled]()

      val admissions = Nel.of(
        Admission(primaryController.localUri, Some(userAndPassword)),
        Admission(backupController.localUri, Some(userAndPassword)))
      val controllerApi = new ControllerApi(
        admissionsToApiResource(admissions)(primaryController.actorSystem))

      val orderId = OrderId("BIG-ORDER")
      controllerApi
        .addOrders(Observable(FreshOrder(orderId, workflow.path, Map(
          "ARG" -> StringValue(bigString)))))
        .await(99.s).orThrow
      val event = eventWatch.await[OrderTerminated](_.key == orderId)
      assert(event.head.value.event == OrderFinished())

      val controllerState = controllerApi.controllerState.await(99.s).orThrow
      assert(controllerState.clusterState.asInstanceOf[ClusterState.Coupled].setting.activeId ==
        NodeId("Primary"))

      assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)

      controllerApi.stop await 99.s
    }

object BigJsonControllerClusterTest:
  private val agentPath = AgentPath("AGENT")
  private val bigStringSize = 9_000_000 max
    Js7Configuration.defaultConfig.memorySizeAsInt("js7.web.chunk-size").orThrow

  // Function, to keep heap small (for proper a heap dump)
  private def bigString = "+" * bigStringSize

  private class TestJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess.succeeded(Map(
        "RESULT" -> StringValue(bigString)))
  private object TestJob extends InternalJob.Companion[TestJob]
