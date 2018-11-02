package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension ⇒ sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{<-:, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAwaiting, OrderDetachable, OrderFinished, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.OfferAndAwaitOrderTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OfferAndAwaitOrderTest extends FreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      for ((id, workflow) ← TestWorkflows) directoryProvider.master.writeTxt(id.path, workflow)
      for (a ← directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/executable$sh"), ":")

      directoryProvider.run { (master, _) ⇒
        val eventCollector = new TestEventCollector
        eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])

        master.addOrderBlocking(JoinBefore1Order)
        master.addOrderBlocking(JoinBefore2Order)
        eventCollector.await[OrderAwaiting](_.key == JoinBefore1Order.id)
        eventCollector.await[OrderAwaiting](_.key == JoinBefore2Order.id)

        master.addOrderBlocking(OfferedOrder)
        eventCollector.await[OrderJoined]  (_.key == JoinBefore1Order.id)
        eventCollector.await[OrderJoined]  (_.key == JoinBefore2Order.id)
        eventCollector.await[OrderFinished](_.key == JoinBefore1Order.id)
        eventCollector.await[OrderFinished](_.key == JoinBefore2Order.id)

        master.addOrderBlocking(JoinAfterOrder)
        eventCollector.await[OrderJoined]  (_.key == JoinAfterOrder.id)
        eventCollector.await[OrderFinished](_.key == JoinAfterOrder.id)

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
        assert(keyedEvents.collect { case OfferedOrderId <-: event ⇒ event }.map(cleanPublishedUntil) == ExpectedPublishingOrderEvents)
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
}

object OfferAndAwaitOrderTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val JoiningWorkflowId = WorkflowPath("/A") % "(initial)"
  private val PublishingWorkflowId = WorkflowPath("/B") % "(initial)"
  private val TestWorkflows = List(
    JoiningWorkflowId → s"""
      workflow {
        execute executable="/executable$sh", agent="AGENT";
        await orderId = "OFFERED-ORDER-ID";
        execute executable="/executable$sh", agent="AGENT";
      }""",
    PublishingWorkflowId → s"""
      workflow {
        execute executable="/executable$sh", agent="AGENT";
        offer orderId = "OFFERED-ORDER-ID", timeout = 60;
        execute executable="/executable$sh", agent="AGENT";
      }""")

  private val OfferedOrderId = OrderId("🔵")
  private val JoinBefore1Order = FreshOrder(OrderId("🥕"), JoiningWorkflowId.path)
  private val JoinBefore2Order = FreshOrder(OrderId("🍋"), JoiningWorkflowId.path)
  private val JoinAfterOrderId = OrderId("🍭")
  private val JoinAfterOrder = FreshOrder(JoinAfterOrderId, JoiningWorkflowId.path)
  private val OfferedOrder = FreshOrder(OfferedOrderId, PublishingWorkflowId.path)

  private val ExpectedJoiningEvents = Vector(
    OrderAdded(JoiningWorkflowId),
    OrderTransferredToAgent(TestAgentPath % "(initial)"),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(1)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderAwaiting(OrderId("OFFERED-ORDER-ID")),
    OrderJoined(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(2)),
    OrderTransferredToAgent(TestAgentPath % "(initial)"),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
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
    OrderAdded(PublishingWorkflowId),
    OrderTransferredToAgent(TestAgentPath % "(initial)"),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(1)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderOffered(OrderId("OFFERED-ORDER-ID"), TestPublishedUntil),
    OrderMoved(Position(2)),
    OrderTransferredToAgent(TestAgentPath % "(initial)"),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(3)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)
}
