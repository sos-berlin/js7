package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{<-:, EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAwaiting, OrderDetachable, OrderFinished, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.tests.OfferAndAwaitOrderTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class OfferAndAwaitOrderTest extends FreeSpec
{
  "Offer and Await after a job" in {
    val workflows = List(
      WorkflowParser.parse(JoiningWorkflowId, s"""
        define workflow {
          execute executable="/executable$sh", agent="AGENT";
          await orderId = "OFFERED-ORDER-ID";
          execute executable="/executable$sh", agent="AGENT";
        }""").orThrow,
      WorkflowParser.parse(PublishingWorkflowId, s"""
        define workflow {
          execute executable="/executable$sh", agent="AGENT";
          offer orderId = "OFFERED-ORDER-ID", timeout = 60;
          execute executable="/executable$sh", agent="AGENT";
        }""").orThrow)
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflows, testName = Some("OfferAndAwaitOrderTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/executable$sh"), ":")

      directoryProvider.run { (master, _) =>
        runOrders(master)

        checkEventSeq(master.eventWatch.all[OrderEvent],
          expectedOffering = Vector(
              OrderAdded(PublishingWorkflowId),
              OrderAttachable(TestAgentRefPath),
              OrderTransferredToAgent(TestAgentRefPath),
              OrderStarted,
              OrderProcessingStarted,
              OrderProcessed(Outcome.succeeded),
              OrderMoved(Position(1)),
              OrderDetachable,
              OrderTransferredToMaster,
              OrderOffered(OrderId("OFFERED-ORDER-ID"), TestPublishedUntil),
              OrderMoved(Position(2)),
              OrderAttachable(TestAgentRefPath),
              OrderTransferredToAgent(TestAgentRefPath),
              OrderProcessingStarted,
              OrderProcessed(Outcome.succeeded),
              OrderMoved(Position(3)),
              OrderDetachable,
              OrderTransferredToMaster,
              OrderFinished),
          expectedAwaiting = Vector(
            OrderAdded(JoiningWorkflowId),
            OrderAttachable(TestAgentRefPath),
            OrderTransferredToAgent(TestAgentRefPath),
            OrderStarted,
            OrderProcessingStarted,
            OrderProcessed(Outcome.succeeded),
            OrderMoved(Position(1)),
            OrderDetachable,
            OrderTransferredToMaster,
            OrderAwaiting(OrderId("OFFERED-ORDER-ID")),
            OrderJoined(Outcome.succeeded),
            OrderMoved(Position(2)),
            OrderAttachable(TestAgentRefPath),
            OrderTransferredToAgent(TestAgentRefPath),
            OrderProcessingStarted,
            OrderProcessed(Outcome.succeeded),
            OrderMoved(Position(3)),
            OrderDetachable,
            OrderTransferredToMaster,
            OrderFinished))
      }
    }
  }

  "Offer and Await as first statements in a Workflow" in {
    val workflows = List(
      WorkflowParser.parse(JoiningWorkflowId, s"""
        define workflow {
          await orderId = "OFFERED-ORDER-ID";
        }""").orThrow,
      WorkflowParser.parse(PublishingWorkflowId, s"""
        define workflow {
          offer orderId = "OFFERED-ORDER-ID", timeout = 60;
        }""").orThrow)
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflows, testName = Some("OfferAndAwaitOrderTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/executable$sh"), ":")

      directoryProvider.run { (master, _) =>
        runOrders(master)

        checkEventSeq(master.eventWatch.all[OrderEvent],
          expectedOffering = Vector(
              OrderAdded(PublishingWorkflowId),
              OrderStarted,
              OrderOffered(OrderId("OFFERED-ORDER-ID"), TestPublishedUntil),
              OrderMoved(Position(1)),
              OrderFinished),
          expectedAwaiting = Vector(
            OrderAdded(JoiningWorkflowId),
            OrderStarted,
            OrderAwaiting(OrderId("OFFERED-ORDER-ID")),
            OrderJoined(Outcome.succeeded),
            OrderMoved(Position(1)),
            OrderFinished))
      }
    }
  }

  private def runOrders(master: RunningMaster): Unit = {
    master.addOrderBlocking(JoinBefore1Order)
    master.addOrderBlocking(JoinBefore2Order)
    master.eventWatch.await[OrderAwaiting](_.key == JoinBefore1Order.id)
    master.eventWatch.await[OrderAwaiting](_.key == JoinBefore2Order.id)

    master.addOrderBlocking(OfferedOrder)
    master.eventWatch.await[OrderJoined]  (_.key == JoinBefore1Order.id)
    master.eventWatch.await[OrderJoined]  (_.key == JoinBefore2Order.id)
    master.eventWatch.await[OrderFinished](_.key == JoinBefore1Order.id)
    master.eventWatch.await[OrderFinished](_.key == JoinBefore2Order.id)

    master.addOrderBlocking(JoinAfterOrder)
    master.eventWatch.await[OrderJoined]  (_.key == JoinAfterOrder.id)
    master.eventWatch.await[OrderFinished](_.key == JoinAfterOrder.id)
  }

  private def checkEventSeq(eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]], expectedOffering: Seq[OrderEvent], expectedAwaiting: Seq[OrderEvent]): Unit =
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val keyedEvents = stampeds.map(_.value).toVector
        for (orderId <- Array(JoinBefore1Order.id, JoinBefore2Order.id, JoinAfterOrder.id)) {
          assert(keyedEvents.collect { case `orderId` <-: event => event } == expectedAwaiting, s" - $orderId")
        }
        assert(keyedEvents.collect { case OfferedOrderId <-: event => event }.map(cleanPublishedUntil) == expectedOffering)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
}

object OfferAndAwaitOrderTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val JoiningWorkflowId = WorkflowPath("/A") ~ "INITIAL"
  private val PublishingWorkflowId = WorkflowPath("/B") ~ "INITIAL"

  private val OfferedOrderId = OrderId("🔵")
  private val JoinBefore1Order = FreshOrder(OrderId("🥕"), JoiningWorkflowId.path)
  private val JoinBefore2Order = FreshOrder(OrderId("🍋"), JoiningWorkflowId.path)
  private val JoinAfterOrderId = OrderId("🍭")
  private val JoinAfterOrder = FreshOrder(JoinAfterOrderId, JoiningWorkflowId.path)
  private val OfferedOrder = FreshOrder(OfferedOrderId, PublishingWorkflowId.path)


  private val TestPublishedUntil = Timestamp.ofEpochMilli(777)

  private def cleanPublishedUntil(event: OrderEvent): OrderEvent =
    event match {
      case OrderOffered(o, _) => OrderOffered(o, TestPublishedUntil)
      case o => o
    }
}
