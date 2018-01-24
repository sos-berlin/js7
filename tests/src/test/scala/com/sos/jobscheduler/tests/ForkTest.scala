package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobXml}
import com.sos.jobscheduler.tests.ForkTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

final class ForkTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(AAgentPath, BAgentPath))) { directoryProvider ⇒
      withCloser { implicit closer ⇒
        directoryProvider.master.jsonFile(TestNamedWorkflow.path).contentString = TestWorkflow.asJson.toPrettyString
        for (a ← directoryProvider.agents) a.job(TestJobPath).xml = jobXml(100.ms)

        directoryProvider.runAgents { _ ⇒
          directoryProvider.runMaster { master ⇒
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
}

object ForkTest {
  private val TestOrder = Order(OrderId("🔺"), TestNamedWorkflow.path, state = Order.StartNow, payload = Payload(Map("VARIABLE" → "VALUE")))
  private val XOrderId = OrderId(s"🔺/🥕")
  private val YOrderId = OrderId(s"🔺/🍋")
  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestNamedWorkflow.path, Order.StartNow, Payload(Map("VARIABLE" → "VALUE"))),
    TestOrder.id <-: OrderTransferredToAgent(AAgentPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    TestOrder.id <-: OrderMoved(Position(1)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))), YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
      XOrderId <-: OrderMoved(Position(1, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(1, "🍋", 1)),
    YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderTransferredToAgent(BAgentPath),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))), YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
      XOrderId <-: OrderMoved(Position(1, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(1, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    TestOrder.id <-: OrderMoved(Position(3)),
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))), YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
      XOrderId <-: OrderMoved(Position(3, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(3, "🍋", 1)),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))), YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
      XOrderId <-: OrderMoved(Position(3, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(3, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    TestOrder.id <-: OrderMoved(Position(4)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    TestOrder.id <-: OrderMoved(Position(5)),
  //TestOrder.id <-: OrderDetachable,
  //TestOrder.id <-: OrderTransferredToMaster,

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderTransferredToAgent(BAgentPath),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))), YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
      XOrderId <-: OrderMoved(Position(5, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(5, "🍋", 1)),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))), YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
      XOrderId <-: OrderMoved(Position(5, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(5, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    TestOrder.id <-: OrderMoved(Position(6)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.Good(ReturnCode(0))),
    TestOrder.id <-: OrderMoved(Position(7)),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderFinished)
}
