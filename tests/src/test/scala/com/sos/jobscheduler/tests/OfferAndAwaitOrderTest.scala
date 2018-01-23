package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{<-:, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAwaiting, OrderDetachable, OrderFinished, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.{JobPath, Position, WorkflowPath}
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.tests.OfferAndAwaitOrderTest._
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class OfferAndAwaitOrderTest extends FreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      for ((path, workflow) ← TestWorkflows) directoryProvider.master.txtFile(path).contentString = workflow
      for (a ← directoryProvider.agents) a.job(TestJobPath).xml = <job><script language="shell">exit</script></job>

      directoryProvider.run { (master, _) ⇒
        val eventCollector = new TestEventCollector
        eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])

        master.addOrder(JoinBefore1Order) await 99.s
        master.addOrder(JoinBefore2Order) await 99.s
        eventCollector.await[OrderAwaiting](_.key == JoinBefore1Order.id)
        eventCollector.await[OrderAwaiting](_.key == JoinBefore2Order.id)

        master.addOrder(PublishOrder) await 99.s
        eventCollector.await[OrderJoined]       (_.key == JoinBefore1Order.id)
        eventCollector.await[OrderJoined]       (_.key == JoinBefore2Order.id)
        eventCollector.await[OrderFinished.type](_.key == JoinBefore1Order.id)
        eventCollector.await[OrderFinished.type](_.key == JoinBefore2Order.id)

        master.addOrder(JoinAfterOrder) await 99.s
        eventCollector.await[OrderJoined]       (_.key == JoinAfterOrder.id)
        eventCollector.await[OrderFinished.type](_.key == JoinAfterOrder.id)

        checkEventSeq(eventCollector.all[OrderEvent])
      }
    }
  }

  private def checkEventSeq(eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit =
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val keyedEvents = stampeds.map(_.value).toVector
        for (orderId ← Array(JoinBefore1Order.id, JoinBefore2Order.id, JoinAfterOrder.id)) {
          assert(keyedEvents.collect { case `orderId` <-: event ⇒ event } == ExpectedJoiningEvents, s" - $orderId")
        }
        assert(keyedEvents.collect { case PublishOrderId <-: event ⇒ event }.map(cleanPublishedUntil) == ExpectedPublishingOrderEvents)
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
}

object OfferAndAwaitOrderTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestJobPath = JobPath("/JOB")
  private val JoiningWorkflowPath = WorkflowPath("/A")
  private val PublishingWorkflowPath = WorkflowPath("/B")
  private val TestWorkflows = List(
    JoiningWorkflowPath → """
       |job "JOB" on "AGENT";
       |await orderId = "OFFERED-ORDER-ID";
       |job "JOB" on "AGENT";
       |""".stripMargin,
    PublishingWorkflowPath → """
       |job "JOB" on "AGENT";
       |offer orderId = "OFFERED-ORDER-ID", timeout = 60;
       |job "JOB" on "AGENT";
       |""".stripMargin)

  private val PublishOrderId = OrderId("🔵")
  private val JoinBefore1Order = Order(OrderId("🥕"), JoiningWorkflowPath, state = Order.StartNow)
  private val JoinBefore2Order = Order(OrderId("🍋"), JoiningWorkflowPath, state = Order.StartNow)
  private val JoinAfterOrderId = OrderId("🍭")
  private val JoinAfterOrder = Order(JoinAfterOrderId, JoiningWorkflowPath, state = Order.StartNow)
  private val PublishOrder = Order(PublishOrderId, PublishingWorkflowPath, state = Order.StartNow)

  private val ExpectedJoiningEvents = Vector(
    OrderAdded(JoiningWorkflowPath, Order.StartNow),
    OrderTransferredToAgent(TestAgentPath),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(1)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderAwaiting(OrderId("OFFERED-ORDER-ID")),
    OrderJoined(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(2)),
    OrderTransferredToAgent(TestAgentPath),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)

  private val TestPublishedUntil = Timestamp.ofEpochMilli(777)

  private def cleanPublishedUntil(event: OrderEvent): OrderEvent =
    event match {
      case OrderOffered(o, _) ⇒ OrderOffered(o, TestPublishedUntil)
      case o ⇒ o
    }

  private val ExpectedPublishingOrderEvents = Vector(
    OrderAdded(PublishingWorkflowPath, Order.StartNow),
    OrderTransferredToAgent(TestAgentPath),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(1)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderOffered(OrderId("OFFERED-ORDER-ID"), TestPublishedUntil),
    OrderMoved(Position(2)),
    OrderTransferredToAgent(TestAgentPath),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Good(true)),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)
}
