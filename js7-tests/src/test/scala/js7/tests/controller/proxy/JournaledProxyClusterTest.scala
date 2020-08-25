package js7.tests.controller.proxy

import io.circe.Encoder
import io.circe.syntax._
import js7.base.circeutils.CirceUtils._
import js7.base.eventbus.StandardEventBus
import js7.base.generic.Completed
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.{itemsPerSecondString, measureTimeOfSingleRun}
import js7.base.time.{Stopwatch, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.HttpClient
import js7.common.http.AkkaHttpClient
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.common.utils.ByteUnits.toKBGB
import js7.controller.client.{AkkaHttpControllerApi, HttpControllerApi}
import js7.controller.data.ControllerCommand.TakeSnapshot
import js7.data.controller.ControllerItems.jsonCodec
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.{InventoryItem, UpdateRepoOperation, VersionId}
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.ControllerApi
import js7.proxy.data.ProxyEvent
import js7.proxy.data.event.ProxyStarted
import js7.proxy.javaapi.data.auth.{JAdmission, JHttpsConfig}
import js7.tests.controller.proxy.ClusterProxyTest.{backupUserAndPassword, primaryCredentials, primaryUserAndPassword, workflow}
import js7.tests.controller.proxy.JournaledProxyClusterTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe._

final class JournaledProxyClusterTest extends AnyFreeSpec with ClusterProxyTest
{
  private implicit def implicitActorSystem = actorSystem

  "JournaledProxy[ControllerState]" in {
    runControllerAndBackup() { (_, primaryController, _, backupController) =>
      val controllerApiResources = List(
        AkkaHttpControllerApi.resource(primaryController.localUri, Some(primaryUserAndPassword), name = "JournaledProxy-Primary"),
        AkkaHttpControllerApi.resource(backupController.localUri, Some(backupUserAndPassword), name = "JournaledProxy-Backup"))
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
    runControllerAndBackup() { (_, _, _, _) =>
      val admissions = List(JAdmission.of(s"http://127.0.0.1:$primaryControllerPort", primaryCredentials)).asJava
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
      val controllerApiResource = AkkaHttpControllerApi.resource(primaryController.localUri, Some(primaryUserAndPassword), name = "JournaledProxy")
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
          workflowPaths :+ ClusterProxyTest.workflow.path).sorted)
      } finally proxy.stop await 99.s

      locally {
        // MEASURE SNAPSHOT TIMES
        var t = now
        api.executeCommand(TakeSnapshot).await(99.s).orThrow
        logger.info(s"TakeSnapshot: ${itemsPerSecondString(t.elapsed, n, "items")}")

        t = now
        val es = api.eventObservable(new StandardEventBus[ProxyEvent], eventId = Some(primaryController.eventWatch.lastAddedEventId))
          .headL
          .await(99.s)
        logger.info(s"Fetch snapshot: ${itemsPerSecondString(t.elapsed, n, "items")}")
        assert(es.stampedEvent.value.event == ProxyStarted)
        assert(es.state.repo.currentVersionSize == n + 2)
      }
    }
  }

  "addOrders" in {
    val bigOrder = FreshOrder(OrderId("ORDER"), workflow.path, Some(Timestamp("2100-01-01T00:00:00Z")),
      arguments = Map("A" -> "*" * 800))
    val n = calculateNumberOf(Stamped(0L, bigOrder.toOrderAdded(workflow.id.versionId): KeyedEvent[OrderEvent]))
    runControllerAndBackup() { (_, primaryController, _, _) =>
      val api = new ControllerApi(List(
        AkkaHttpControllerApi.resource(primaryController.localUri, Some(primaryUserAndPassword), name = "JournaledProxy")))
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

  "addOrder, long stream containing an invalid item returns proper error message" in {
    // The server must be able to respond with an error even if the posted stream has not completedly processed.
    val order = FreshOrder(OrderId("ORDER"), workflow.path).asJson.compactPrint
    val bigOrder = order + " "*(990_000 - order.length)
    val n = 100_000_000/*bytes*/ / bigOrder.length
    val orders = Observable.range(1, n + 1).map(_ => bigOrder)
    runControllerAndBackup() { (_, primaryController, _, _) =>
      logger.info(s"Adding $n invalid orders à ${bigOrder.length} bytes ${toKBGB(n * bigOrder.length)}")
      val httpClient = new AkkaHttpClient.Standard(primaryController.localUri, HttpControllerApi.UriPrefixPath, actorSystem,
        name = "JournaledProxy")
      autoClosing(httpClient) { _ =>
        val api = new HttpControllerApi.Standard(primaryController.localUri, Some(primaryUserAndPassword), httpClient)
        api.login().await(99.s)
        locally {
          val response = HttpClient.liftProblem(
            api.postObservableJsonString("controller/api/order", orders)
              .map(_ => Completed)
          ).await(99.s)
          assert(response == Left(Problem("Unexpected duplicates: Order:ORDER")))
        }

        locally {
          val response = HttpClient.liftProblem(
            api.postObservableJsonString("controller/api/order", orders.map("¿" + _))
              .map(_ => Completed)
          ).await(99.s)
          assert(response == Left(Problem("JSON ParsingFailure: expected json value got '¿{\"id\"...' (line 1, column 1)")))
        }
      }
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

object JournaledProxyClusterTest
{
  private val logger = Logger(getClass)
}
