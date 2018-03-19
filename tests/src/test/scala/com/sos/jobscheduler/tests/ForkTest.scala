package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobXml}
import com.sos.jobscheduler.tests.ForkTest._
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

final class ForkTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(AAgentPath, BAgentPath))) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        directoryProvider.master.writeJson(TestWorkflow.withoutVersion)
        for (a ← directoryProvider.agents) a.file(TestJobPath, SourceType.Xml).xml = jobXml(100.ms)

        directoryProvider.runAgents() { _ ⇒
          directoryProvider.runMaster() { master ⇒
            val eventCollector = new TestEventCollector
            eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
            master.addOrder(TestOrder) await 99.s
            eventCollector.await[OrderFinished](_.key == TestOrder.id)
            checkEventSeq(eventCollector.all[OrderEvent])
          }
        }
      }
    }
  }

  private def checkEventSeq(eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val keyedEvents = stampeds.map(_.value).toVector
        for (orderId ← Array(TestOrder.id, XOrderId, YOrderId)) {  // But ordering if each order is determined
          assert(keyedEvents.filter(_.key == orderId) == ExpectedEvents.filter(_.key == orderId))
        }
        assert(keyedEvents.toSet == ExpectedEvents.toSet)  // XOrderId and YOrderId run in parallel and ordering is not determined
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }

  "JSON" in {
    testJson(ForkTestSetting.TestWorkflow, json"""{
      "id": {
         "path": "/WORKFLOW",
         "versionId": "(initial)"
       },
      "source": "job \"JOB\" on \"AGENT-A\";\nfork(\n  \"🥕\" { job \"JOB\" on \"AGENT-A\"; job \"JOB\" on \"AGENT-A\"; },\n  \"🍋\" { job \"JOB\" on \"AGENT-A\"; job \"JOB\" on \"AGENT-B\"; });\njob \"JOB\" on \"AGENT-A\";\nfork(\n  \"🥕\" { job \"JOB\" on \"AGENT-A\"; job \"JOB\" on \"AGENT-A\"; },\n  \"🍋\" { job \"JOB\" on \"AGENT-A\"; job \"JOB\" on \"AGENT-A\"; });\njob \"JOB\" on \"AGENT-A\";\nfork(\n  \"🥕\" { job \"JOB\" on \"AGENT-A\"; job \"JOB\" on \"AGENT-A\"; },\n  \"🍋\" { job \"JOB\" on \"AGENT-B\"; job \"JOB\" on \"AGENT-B\"; });\njob \"JOB\" on \"AGENT-A\";",
      "instructions": [
        { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
        {
          "TYPE": "ForkJoin",
          "branches": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-B" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
        {
          "TYPE": "ForkJoin",
          "branches": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
        {
          "TYPE": "ForkJoin",
          "branches": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" },
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-B" },
                  { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-B" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT-A" }
      ]
    }""")
  }
}

object ForkTest {
  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path, payload = Payload(Map("VARIABLE" → "VALUE")))
  private val XOrderId = OrderId(s"🔺/🥕")
  private val YOrderId = OrderId(s"🔺/🍋")
  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestWorkflow.id, None, Payload(Map("VARIABLE" → "VALUE"))),
    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(1)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(1, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(1, "🍋", 1)),
    YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderTransferredToAgent(BAgentPath % "(initial)"),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(1, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(1, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(3)),
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(3, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(3, "🍋", 1)),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(3, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(3, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(4)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(5)),
  //TestOrder.id <-: OrderDetachable,
  //TestOrder.id <-: OrderTransferredToMaster,

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderTransferredToAgent(BAgentPath % "(initial)"),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(5, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(5, "🍋", 1)),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(5, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(5, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(6)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(7)),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderFinished)
}
