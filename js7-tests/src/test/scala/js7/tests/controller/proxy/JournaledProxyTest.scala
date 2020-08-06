package js7.tests.controller.proxy

import io.circe.Encoder
import io.circe.syntax._
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTimeOfSingleRun
import js7.base.time.{Stopwatch, Timestamp}
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.client.AkkaHttpControllerApi
import js7.data.controller.ControllerItems.jsonCodec
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.{InventoryItem, UpdateRepoOperation, VersionId}
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.ControllerApi
import js7.proxy.javaapi.JAdmission
import js7.proxy.javaapi.data.JHttpsConfig
import js7.tests.controller.proxy.ClusterProxyTest.{backupUserAndPassword, primaryCredentials, primaryUserAndPassword}
import js7.tests.controller.proxy.JournaledProxyTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe._

final class JournaledProxyTest extends AnyFreeSpec with ClusterProxyTest
{
  protected val inventoryItems = workflow :: Nil

  "JournaledProxy[ControllerState]" in {
    runControllerAndBackup() { (_, primaryController, _, backupController) =>
      val controllerApiResources = List(
        AkkaHttpControllerApi.separateAkkaResource(primaryController.localUri, Some(primaryUserAndPassword), name = "JournaledProxy-Primary"),
        AkkaHttpControllerApi.separateAkkaResource(backupController.localUri, Some(backupUserAndPassword), name = "JournaledProxy-Backup"))
      val proxy = new ControllerApi(controllerApiResources).startProxy().await(99.s)
      try {
        val whenProcessed = proxy.eventBus.when[OrderProcessed].runToFuture
        val whenFinished = proxy.eventBus.when[OrderFinished.type].runToFuture
        primaryController.addOrder(FreshOrder(OrderId("🔺"), workflow.id.path)).runAsyncAndForget

        val processed = whenProcessed.await(99.s)
        assert(processed.stampedEvent.value.event == OrderProcessed(Outcome.succeeded))
        assert(processed.state.idToOrder(OrderId("🔺")).state == Order.Processed)

        whenFinished await 99.s  // Await order termination before shutting down the JS7
      } finally proxy.stop await 99.s
    }
  }

  "JControllerProxy with Flux" in {
    val List(primaryPort, backupPort) = findFreeTcpPorts(2)
    runControllerAndBackup(primaryHttpPort = primaryPort, backupHttpPort = backupPort) { (_, _, _, _) =>
      val admissions = List(JAdmission.of(s"http://127.0.0.1:$primaryPort", primaryCredentials)).asJava
      val tester = new JControllerFluxTester(admissions, JHttpsConfig.empty)
      tester.test()
      tester.close()
    }
  }

  "updateRepo" in {
    val versionId = VersionId("MY-VERSION")
    val workflow = WorkflowParser.parse(s"""
      define workflow {
        execute executable="/path-to-my-script", agent="/AGENT",
          arguments = { "A": "${"A" * 700}" };
      }""").orThrow.withoutSource
    val n = calculateNumberOf[InventoryItem](workflow.withId(WorkflowPath("/WORKFLOW-XXXXX") ~ versionId))
    runControllerAndBackup() { (primary, primaryController, _, _) =>
      val controllerApiResource = AkkaHttpControllerApi.separateAkkaResource(primaryController.localUri, Some(primaryUserAndPassword), name = "JournaledProxy")
      val api = new ControllerApi(controllerApiResource :: Nil)
      val workflowPaths = (1 to n).map(i => WorkflowPath(s"/WORKFLOW-$i"))
      val sw = new Stopwatch
      val operations = Observable.fromIterable(workflowPaths)
        .mapParallelUnordered(sys.runtime.availableProcessors)(path => Task(
          UpdateRepoOperation.AddOrReplace(
            primary.sign(workflow.copy(id = path ~ versionId)))))
          .toListL await 99.s
      logger.info(sw.itemsPerSecondString(n, "signatures"))
      val logLine = measureTimeOfSingleRun(n, "workflows") {
        api.updateRepo(versionId, Observable.fromIterable(operations))
          .await(999.s).orThrow
      }
      logger.info(logLine.toString)
      // TODO Await Event
      val proxy = api.startProxy().await(99.s)
      try {
        waitForCondition(30.s, 50.ms)(proxy.currentState.repo.currentTyped[Workflow].sizeIs == n + 1)
        assert(proxy.currentState.repo.currentTyped[Workflow].keys.toVector.sorted == (
          workflowPaths :+ JournaledProxyTest.workflow.path).sorted)
      } finally proxy.stop await 99.s
    }
  }

  "addOrders" in {
    val bigOrder = FreshOrder(OrderId("ORDER"), JournaledProxyTest.workflow.path, Some(Timestamp("2100-01-01T00:00:00Z")),
      arguments = Map("A" -> "*" * 800))
    val n = calculateNumberOf(Stamped(0L, bigOrder.toOrderAdded(workflow.id.versionId): KeyedEvent[OrderEvent]))
    runControllerAndBackup() { (_, primaryController, _, _) =>
      val controllerApiResource = AkkaHttpControllerApi.separateAkkaResource(primaryController.localUri, Some(primaryUserAndPassword), name = "JournaledProxy")
      val api = new ControllerApi(controllerApiResource :: Nil)
      val orderIds = (1 to n).map(i => OrderId(s"ORDER-$i"))
      val logLine = measureTimeOfSingleRun(n, "orders") {
        api.addOrders(
          Observable.fromIterable(orderIds)
            .map(orderId => bigOrder.copy(id = orderId))
        ).await(199.s).orThrow
      }
      logger.info(logLine.toString)
      val proxy = api.startProxy().await(99.s)
      try {
        waitForCondition(30.s, 50.ms)(proxy.currentState.idToOrder.sizeIs == n)
        assert(proxy.currentState.idToOrder.keys.toVector.sorted == orderIds.sorted)
      } finally proxy.stop await 99.s
    }
  }

  private def calculateNumberOf[A: Encoder: TypeTag](sample: A): Int =
    if (sys.props.contains("test.speed") && sys.runtime.maxMemory >= 16_000_000_000L) {
      val sampleSize = sample.asJson.compactPrint.length
      val n = 100_000
      logger.info(s"$n× ${implicitly[TypeTag[A]].tpe} à $sampleSize bytes = ${n * sampleSize} bytes")
      logger.info(sample.asJson.compactPrint)
      n
    } else
      1000

  "tornOlder" in {
    pending
  }
}

object JournaledProxyTest
{
  private val logger = Logger(getClass)
  private[proxy] val workflow = WorkflowParser.parse(
    WorkflowPath("/WORKFLOW") ~ "INITIAL",
    s"""
      define workflow {
        execute executable="/TEST.cmd", agent="AGENT", taskLimit=10;
      }"""
  ).orThrow
}
