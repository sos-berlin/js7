package js7.tests

import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.problem.Checked.Ops
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.event.{<-:, EventSeq, KeyedEvent, TearableEventSeq}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAwaiting, OrderDetachable, OrderDetached, OrderFinished, OrderJoined, OrderMoved, OrderOffered, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.OfferAndAwaitOrderTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class OfferAndAwaitOrderTest extends AnyFreeSpec
{
  "Offer and Await after a job" in {
    pending // TODO
    val workflows = List(
      WorkflowParser.parse(JoiningWorkflowId, s"""
        define workflow {
          execute executable="executable$sh", agent="AGENT";
          await orderId = "OFFERED-ORDER-ID";
          execute executable="executable$sh", agent="AGENT";
        }""").orThrow,
      WorkflowParser.parse(OfferingWorkflowId, s"""
        define workflow {
          execute executable="executable$sh", agent="AGENT";
          offer orderId = "OFFERED-ORDER-ID", timeout = 60;
          execute executable="executable$sh", agent="AGENT";
        }""").orThrow)
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, workflows, testName = Some("OfferAndAwaitOrderTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(RelativePathExecutable(s"executable$sh"), ":")

      directoryProvider.run { (controller, _) =>
        runOrders(controller)

        checkEventSeq(controller.eventWatch.all[OrderEvent],
          expectedOffering = Vector(
              OrderAdded(OfferingWorkflowId),
              OrderAttachable(TestAgentPath),
              OrderAttached(TestAgentPath),
              OrderStarted,
              OrderProcessingStarted,
              OrderProcessed(Outcome.succeededRC0),
              OrderMoved(Position(1)),
              OrderDetachable,
              OrderDetached,
              OrderOffered(OrderId("OFFERED-ORDER-ID"), TestOfferedUntil),
              OrderMoved(Position(2)),
              OrderAttachable(TestAgentPath),
              OrderAttached(TestAgentPath),
              OrderProcessingStarted,
              OrderProcessed(Outcome.succeededRC0),
              OrderMoved(Position(3)),
              OrderDetachable,
              OrderDetached,
              OrderFinished),
          expectedAwaiting = Vector(
            OrderAdded(JoiningWorkflowId),
            OrderAttachable(TestAgentPath),
            OrderAttached(TestAgentPath),
            OrderStarted,
            OrderProcessingStarted,
            OrderProcessed(Outcome.succeededRC0),
            OrderMoved(Position(1)),
            OrderDetachable,
            OrderDetached,
            OrderAwaiting(OrderId("OFFERED-ORDER-ID")),
            OrderJoined(Outcome.succeeded),
            OrderMoved(Position(2)),
            OrderAttachable(TestAgentPath),
            OrderAttached(TestAgentPath),
            OrderProcessingStarted,
            OrderProcessed(Outcome.succeededRC0),
            OrderMoved(Position(3)),
            OrderDetachable,
            OrderDetached,
            OrderFinished))
      }
    }
  }

  "Offer and Await as first statements in a Workflow" in {
    pending // TODO
    val workflows = List(
      WorkflowParser.parse(JoiningWorkflowId, s"""
        define workflow {
          await orderId = "OFFERED-ORDER-ID";
        }""").orThrow,
      WorkflowParser.parse(OfferingWorkflowId, s"""
        define workflow {
          offer orderId = "OFFERED-ORDER-ID", timeout = 60;
        }""").orThrow)
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, workflows, testName = Some("OfferAndAwaitOrderTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) a.writeExecutable(RelativePathExecutable(s"executable$sh"), ":")

      directoryProvider.run { (controller, _) =>
        runOrders(controller)

        checkEventSeq(controller.eventWatch.all[OrderEvent],
          expectedOffering = Vector(
              OrderAdded(OfferingWorkflowId),
              OrderStarted,
              OrderOffered(OrderId("OFFERED-ORDER-ID"), TestOfferedUntil),
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

  private def runOrders(controller: RunningController): Unit = {
    controller.addOrderBlocking(JoinBefore1Order)
    controller.addOrderBlocking(JoinBefore2Order)
    controller.eventWatch.await[OrderAwaiting](_.key == JoinBefore1Order.id)
    controller.eventWatch.await[OrderAwaiting](_.key == JoinBefore2Order.id)

    controller.addOrderBlocking(OfferingOrder)
    controller.eventWatch.await[OrderJoined]  (_.key == JoinBefore1Order.id)
    controller.eventWatch.await[OrderJoined]  (_.key == JoinBefore2Order.id)
    controller.eventWatch.await[OrderFinished](_.key == JoinBefore1Order.id)
    controller.eventWatch.await[OrderFinished](_.key == JoinBefore2Order.id)

    controller.addOrderBlocking(JoinAfterOrder)
    controller.eventWatch.await[OrderJoined]  (_.key == JoinAfterOrder.id)
    controller.eventWatch.await[OrderFinished](_.key == JoinAfterOrder.id)
  }

  private def checkEventSeq(eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]], expectedOffering: Seq[OrderEvent], expectedAwaiting: Seq[OrderEvent]): Unit =
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val keyedEvents = stampeds.iterator.map(_.value).to(Vector)
        for (orderId <- Array(JoinBefore1Order.id, JoinBefore2Order.id, JoinAfterOrder.id)) {
          assert(keyedEvents.collect { case `orderId` <-: event => event } == expectedAwaiting, s" - $orderId")
        }
        assert(keyedEvents.collect { case OfferingOrder.id <-: event => event }.map(cleanOfferedUntil) == expectedOffering)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
}

object OfferAndAwaitOrderTest
{
  private val TestAgentPath = AgentPath("AGENT")
  private val JoiningWorkflowId = WorkflowPath("A") ~ "INITIAL"
  private val OfferingWorkflowId = WorkflowPath("B") ~ "INITIAL"

  private val JoinBefore1Order = FreshOrder(OrderId("🥕"), JoiningWorkflowId.path)
  private val JoinBefore2Order = FreshOrder(OrderId("🍋"), JoiningWorkflowId.path)
  private val JoinAfterOrder = FreshOrder(OrderId("🍭"), JoiningWorkflowId.path)
  private val OfferingOrder = FreshOrder(OrderId("🔵"), OfferingWorkflowId.path)

  private val TestOfferedUntil = Timestamp.ofEpochMilli(777)

  private def cleanOfferedUntil(event: OrderEvent): OrderEvent =
    event match {
      case OrderOffered(o, _) => OrderOffered(o, TestOfferedUntil)
      case o => o
    }
}
