package js7.tests.controller.proxy

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import fs2.Stream
import io.circe.Encoder
import io.circe.syntax.*
import izumi.reflect.Tag
import js7.base.auth.Admission
import js7.base.circeutils.CirceUtils.*
import js7.base.eventbus.StandardEventBus
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.headL
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.{itemsPerSecondString, measureTimeOfSingleRun}
import js7.base.time.TimestampForTests.ts
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.time.{Stopwatch, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.CatsBlocking.*
import js7.base.utils.CatsUtils.Nel
import js7.base.web.HttpClient
import js7.common.http.PekkoHttpClient
import js7.controller.client.{HttpControllerApi, PekkoHttpControllerApi}
import js7.data.controller.ControllerCommand.TakeSnapshot
import js7.data.controller.ControllerState
import js7.data.controller.ControllerState.versionedItemJsonCodec
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.ItemOperation.AddVersion
import js7.data.item.{ItemOperation, VersionId, VersionedItem}
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.StringValue
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.journal.watch.StrictEventWatch
import js7.proxy.ControllerApi
import js7.proxy.data.event.{ProxyEvent, ProxyStarted}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.controller.proxy.ClusterProxyTest.{backupUserAndPassword, primaryCredentials, primaryUserAndPassword, workflow}
import js7.tests.controller.proxy.JournaledProxyClusterTest.*
import org.apache.pekko.actor.ActorSystem
import scala.concurrent.duration.Deadline.now
import scala.jdk.CollectionConverters.*

final class JournaledProxyClusterTest extends OurTestSuite, ClusterProxyTest:

  private given IORuntime = ioRuntime
  private implicit def implicitActorSystem: ActorSystem = actorSystem

  "JournaledProxy[ControllerState]" in:
    runControllerAndBackup(): (_, primaryController, _, _, backupController, _, _) =>
      primaryController.waitUntilReady()
      val controllerApiResources = Nel
        .of(
          PekkoHttpControllerApi.resource(
            Admission(primaryController.localUri, Some(primaryUserAndPassword)),
            name = "JournaledProxy-Primary"),
          PekkoHttpControllerApi.resource(
            Admission(backupController.localUri, Some(backupUserAndPassword)),
            name = "JournaledProxy-Backup"))
        .sequence

      ControllerApi.resource(controllerApiResources).blockingUse(99.s): api =>
        api.controllerProxy().blockingUse(99.s): proxy =>
          val whenProcessed = proxy.eventBus.when[OrderProcessed]
          val whenFinished = proxy.eventBus.when[OrderFinished]
          primaryController.addOrderBlocking(FreshOrder(OrderId("🔺"), workflow.id.path))

          val processed = whenProcessed.await(99.s)
          assert(processed.stampedEvent.value.event == OrderProcessed(OrderOutcome.succeededRC0))
          assert(processed.state.idToOrder(OrderId("🔺")).state == Order.Processed)

          whenFinished.await(99.s)  // Await order termination before shutting down the JS7

  "JControllerProxy with Flux" in:
    runControllerAndBackup() { (_, primaryController, _, _, _, _, _) =>
      primaryController.waitUntilReady()
      val admissions = List(JAdmission.of(s"http://127.0.0.1:$primaryControllerPort", primaryCredentials)).asJava
      val tester = new JControllerFluxTester(admissions, JHttpsConfig.empty)
      tester.test()
      tester.close()
    }

  "updateItems" in:
    val versionId = VersionId("MY-VERSION")
    val workflow = WorkflowParser.parse(s"""
      define workflow {
        execute executable="path-to-my-script", agent="AGENT",
          defaultArguments = { "A": "${"A" * 700}" };
      }""").orThrow.withoutSource
    val n = calculateNumberOf[VersionedItem](200_000, workflow.withId(WorkflowPath("WORKFLOW-XXXXX") ~ versionId))
    logger.info(s"Adding $n Workflows")

    runControllerAndBackup(): (primary, primaryController, _, _, _, _, _) =>
      primaryController.waitUntilReady()
      val controllerApiResource = PekkoHttpControllerApi.resource(
        Admission(primaryController.localUri, Some(primaryUserAndPassword)),
        name = "JournaledProxy")

      ControllerApi.resource(controllerApiResource map Nel.one).blockingUse(99.s): api =>
        val workflowPaths = (1 to n).map(i => WorkflowPath(s"WORKFLOW-$i"))
        val sw = new Stopwatch
        val operations = Stream.iterable(workflowPaths)
          .mapParallelBatch(): path =>
            ItemOperation.AddOrChangeSigned(
              primary.toSignedString(workflow.copy(id = path ~ versionId)))
          .compile.toList.await(99.s)
        logger.info(sw.itemsPerSecondString(n, "signatures"))
        val logLine = measureTimeOfSingleRun(n, "workflows"):
          api.updateItems(Stream.iterable(AddVersion(versionId) :: operations))
            .await(999.s).orThrow
        logger.info(logLine.toString)
        // TODO Await Event
        api.controllerProxy().blockingUse(99.s): proxy =>
          awaitAndAssert(proxy.currentState.repo.currentTyped[Workflow].sizeIs == n + 1)
          assert(proxy.currentState.repo.currentTyped[Workflow].keys.toVector.sorted == (
            workflowPaths :+ ClusterProxyTest.workflow.path).sorted)

        locally:
          meterTakeSnapshot(n, api)
          val state = meterFetchSnapshot(n, api, primaryController.eventWatch)
          assert(state.repo.currentVersionSize == n + 1)

  "addOrders" in:
    val bigOrder = FreshOrder(OrderId("ORDER"), workflow.path,
      Map("A" -> StringValue("*" * 800)),
      scheduledFor = Some(ts"2100-01-01T00:00:00Z"))
    val n = calculateNumberOf(500_000,
      Stamped(0L,
        bigOrder.toOrderAdded(workflow.id.versionId, bigOrder.arguments): KeyedEvent[OrderEvent]))
    logger.info(s"Adding $n big orders")
    runControllerAndBackup(): (_, primaryController, _, _, _, _, _) =>
      primaryController.waitUntilReady()
      ControllerApi.resource:
        PekkoHttpControllerApi
          .resource(
            Admission(primaryController.localUri, Some(primaryUserAndPassword)),
            name = "JournaledProxy")
          .map(Nel.one)
      .blockingUse(99.s): api =>
        val orderIds = (1 to n).map(i => OrderId(s"ORDER-$i"))
        val logLine = measureTimeOfSingleRun(n, "orders"):
          api.addOrders:
            Stream.iterable(orderIds)
              .map(orderId => bigOrder.copy(id = orderId))
          .await(199.s).orThrow
        logger.info(logLine.toString)
        api.controllerProxy().blockingUse(99.s): proxy =>
          waitForCondition(30.s, 50.ms)(proxy.currentState.idToOrder.sizeIs == n)
          assert(proxy.currentState.idToOrder.keys.toVector.sorted == orderIds.sorted)

        locally:
          meterTakeSnapshot(n, api)
          val state = meterFetchSnapshot(n, api, primaryController.eventWatch)
          assert(state.idToOrder.size == n)

  "addOrder, long stream containing an invalid item returns proper error message" in:
    // The server must be able to respond with an error even if the posted stream has not completedly processed.
    val order = FreshOrder(OrderId("ORDER"), workflow.path).asJson.compactPrint
    val bigOrder = order + " "*(990_000 - order.length)
    val n = 100_000_000/*bytes*/ / bigOrder.length
    logger.info(s"Adding $n orders")
    val orders = Stream.range(0, n).map(_ => bigOrder)
    runControllerAndBackup() { (_, primaryController, _, _, _, _, _) =>
      primaryController.waitUntilReady()
      logger.info(s"Adding $n invalid orders à ${bigOrder.length} bytes ${toKBGB(n * bigOrder.length)}")
      val httpClient = new PekkoHttpClient.Standard(primaryController.localUri, HttpControllerApi.UriPrefixPath, actorSystem,
        name = "JournaledProxy")
      autoClosing(httpClient) { _ =>
        val api = new HttpControllerApi.Standard(
          Admission(primaryController.localUri, Some(primaryUserAndPassword)),
          httpClient)
        api.login().await(99.s)
        locally:
          val response = HttpClient.liftProblem(
            api.postJsonStringStream("controller/api/order", orders)
              .map(_ => Completed)
          ).await(99.s)
          assert(response == Left(Problem(s"Unexpected duplicates: $n×Order:ORDER")))

        locally:
          val response = HttpClient.liftProblem(
            api.postJsonStringStream("controller/api/order", orders.map("¿" + _))
              .map(_ => Completed)
          ).await(99.s)
          assert(response == Left(Problem("JSON ParsingFailure: expected json value got '¿{\"id...' (line 1, column 1)")))
      }
    }

  private def calculateNumberOf[A: Encoder: Tag](n: Int, sample: A): Int =
    if sys.props.contains("test.speed") && sys.runtime.maxMemory >= 16_000_000_000L then
      val sampleSize = sample.asJson.toByteArray.length
      logger.info(s"$n× ${implicitly[Tag[A]].tag} à $sampleSize bytes = ${n * sampleSize} bytes")
      logger.info(sample.asJson.compactPrint)
      n
    else
      1000

  "tornOlder" in:
    pending

  private def meterTakeSnapshot(n: Int, api: ControllerApi): Unit =
    val t = now
    api.executeCommand(TakeSnapshot).await(99.s).orThrow
    logger.info(s"TakeSnapshot: ${itemsPerSecondString(t.elapsed, n, "items")}")

  private def meterFetchSnapshot(n: Int, api: ControllerApi, eventWatch: StrictEventWatch): ControllerState =
    val t = now
    val es = api.eventAndStateStream(new StandardEventBus[ProxyEvent], fromEventId = Some(eventWatch.lastAddedEventId))
      .headL
      .await(99.s)
    logger.info(s"Fetch snapshot: ${itemsPerSecondString(t.elapsed, n, "objects")}")
    assert(es.stampedEvent.value.event == ProxyStarted)
    es.state


object JournaledProxyClusterTest:
  private val logger = Logger[this.type]
