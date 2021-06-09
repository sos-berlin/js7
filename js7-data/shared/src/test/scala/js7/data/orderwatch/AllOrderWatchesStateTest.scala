package js7.data.orderwatch

import js7.base.problem.Problem
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderDeletionMarked}
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.{Arised, ArisedOrHasOrder, HasOrder, Vanished}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

final class AllOrderWatchesStateTest extends AnyFreeSpec
{
  private val v1 = VersionId("1")
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val workflowId = workflowPath ~ v1
  private val aOrderWatch = FileWatch(OrderWatchPath("A-WATCH"),
    workflowPath, AgentPath("AGENT"), "DIRECTORY")
  private val bOrderWatch = aOrderWatch.copy(path = OrderWatchPath("B-WATCH"))
  private var aoss = AllOrderWatchesState.empty

  private def state(name: String): Option[ArisedOrHasOrder] =
    aoss.pathToOrderWatchState(aOrderWatch.path).externalToState.get(ExternalOrderName(name))

  "addOrderWatch" in {
    aoss = aoss.addOrderWatch(aOrderWatch).orThrow
    aoss = aoss.addOrderWatch(bOrderWatch).orThrow
    assert(aoss.addOrderWatch(aOrderWatch) ==
      Left(DuplicateKey("OrderWatchPath", "OrderWatch:A-WATCH")))
  }

  "removeOrderWatch" in {
    val x = bOrderWatch.copy(path = OrderWatchPath("X"))
    val all1 = aoss.addOrderWatch(x).orThrow
    assert(all1.pathToOrderWatchState.contains(x.path))
    val all0 = all1.removeOrderWatch(x.path)
    assert(all0 == aoss)
  }

  "changeOrderWatch" in {
    val a1 = aOrderWatch.copy(directory = "CHANGED")
    val all1 = aoss.changeOrderWatch(a1).orThrow
    assert(all1.pathToOrderWatchState(a1.path) == OrderWatchState(a1))
  }

  "Events on the happy path" - {
    "ExternalOrderArised A, B (and X) --> OrderAdded" in {
      val events = Seq(externalOrderArised("A"), externalOrderArised("X"), externalOrderArised("B"), externalOrderVanished("X"))
      for (event <- events) aoss = aoss.onOrderWatchEvent(event).orThrow
      assert(state("A") == Some(Arised(orderId("A"), arguments("A"))))
      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderAdded("A"), orderAdded("B")))
    }

    "ExternalOrderVanished A, cancels previous Arised if OrderAdded was not emitted" in {
      aoss = aoss.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(state("A") == None)
      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderAdded("B")))
    }

    "ExternalOrderArised A" in {
      aoss = aoss.onOrderWatchEvent(externalOrderArised("A")).orThrow
      assert(aoss
        .pathToOrderWatchState(aOrderWatch.path)
        .externalToState(ExternalOrderName("A")) == Arised(orderId("A"), arguments("A")))
      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderAdded("B"), orderAdded("A")))
    }

    "OrderAdded A, B" in {
      aoss = aoss.onOrderAdded(orderAdded("A")).orThrow
      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderAdded("B")))

      aoss = aoss.onOrderAdded(orderAdded("B")).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "ExternalOrderVanished A => OrderRemovedMarked" in {
      aoss = aoss.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderId("A") <-: OrderDeletionMarked))
    }

    "OrderDeletionMarked A" in {
      aoss = aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeletionMarked).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderDeleted A" in {
      aoss = aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeleted).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "ExternalOrderVanished B => OrderRemovedMarked" in {
      aoss = aoss.onOrderWatchEvent(externalOrderVanished("B")).orThrow
      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderId("B") <-: OrderDeletionMarked))
    }

    "OrderDeletionMarked B" in {
      aoss = aoss.onOrderEvent(externalOrderKey("B"), orderId("B") <-: OrderDeletionMarked).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "ExternalOrderArised B, while B order is running" in {
      aoss = aoss.onOrderWatchEvent(externalOrderArised("B")).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderDeleted B => OrderAdded" in {
      aoss = aoss.onOrderEvent(externalOrderKey("B"), orderId("B") <-: OrderDeleted).orThrow
      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderAdded("B")))
      aoss = aoss.onOrderAdded(orderAdded("B")).orThrow
    }

    "OrderVanished, OrderArised, OrderVanished, OrderArised while order is running" in {
      aoss = aoss.onOrderWatchEvent(externalOrderVanished("B")).orThrow
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))

      aoss = aoss.onOrderWatchEvent(externalOrderArised("B")).orThrow
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Arised(orderId("B"), arguments("B"))))))

      aoss = aoss.onOrderWatchEvent(externalOrderVanished("B")).orThrow
      assert(state("B") == Some(HasOrder(orderId("B"), None)))

      assert(aoss.nextEvents(toVersionId).toSeq == Seq(orderId("B") <-: OrderDeletionMarked))

      aoss = aoss.onOrderWatchEvent(externalOrderArised("B")).orThrow
    }
  }

  "OrderDeletionMarked (by user) when not Vanished" in {
    var a = AllOrderWatchesState.empty
    a = a.addOrderWatch(aOrderWatch).orThrow
    a = a.onOrderWatchEvent(externalOrderArised("C")).orThrow
    a = a.onOrderAdded(orderAdded("C")).orThrow

    a = a.onOrderEvent(externalOrderKey("C"), orderId("C") <-: OrderDeletionMarked).orThrow
    assert(a.onOrderWatchEvent(externalOrderVanished("C")) == Left(Problem(
      """Duplicate ExternalOrderVanished(C), state=HasOrder(Order:file:A-SOURCE:C,Some(VanishedAck))""")))

    a = a.onOrderEvent(externalOrderKey("C"), orderId("C") <-: OrderDeleted).orThrow
    assert(a.nextEvents(toVersionId).isEmpty)

    a = a.onOrderWatchEvent(externalOrderArised("C")).orThrow
    assert(a.nextEvents(toVersionId).toSeq == Seq(orderAdded("C")))
  }

  "Events in illegal order" - {
    "Double ExternalOrderArised" in {
      aoss = aoss.onOrderWatchEvent(externalOrderArised("A")).orThrow
      assert(aoss.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        """Duplicate ExternalOrderArised(A, Map(file -> "/DIR/A")): Arised(Order:file:A-SOURCE:A,Map(file -> "/DIR/A"))""")))
    }

    "Double early ExternalOrderVanished" in {
      aoss = aoss.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(aoss.onOrderWatchEvent(externalOrderVanished("A")) == Left(
        UnknownKeyProblem("ExternalOrderName", "A")))
    }

    "Arised after OrderAdded" in {
      aoss = aoss.onOrderWatchEvent(externalOrderArised("A")).orThrow
      aoss = aoss.onOrderAdded(orderAdded("A")).orThrow

      // Second ExternalOrderArised is allowed here only because the state is same as after a second ExternalOrderVanished.
      // Could be improved but should not happen, anyway.
      aoss.onOrderWatchEvent(externalOrderArised("A")).orThrow
      //assert(aoss.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
      //  """Duplicate ExternalOrderArised(A, Map(file -> "/DIR/A")): HasOrder(Order:file:A-SOURCE:A,None)""")))
    }

    "Vanished before OrderDeletionMarked" in {
      aoss = aoss.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(aoss.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-SOURCE:A,Some(Vanished))""")))
      val a = aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeletionMarked).orThrow

      assert(aoss.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-SOURCE:A,Some(Vanished))""")))
    }

    "Arised without OrderDeletionMarked" in {
      // May happen if OrderDeletionMarked was already emitted
      // - due to explicit command
      // - due to repeated ExternalOrderArised and ExternalOrderVanished
      aoss = aoss.onOrderWatchEvent(externalOrderArised("A")).orThrow
      assert(aoss.pathToOrderWatchState(aOrderWatch.path).externalToState(ExternalOrderName("A")) ==
        HasOrder(orderId("A"), Some(Arised(orderId("A"), order("A").arguments))))

      assert(aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeletionMarked) == Right(aoss))
    }
  }

  private def toVersionId(workflowPath: WorkflowPath) = Some(v1)

  private def arguments(name: String) =
    NamedValues("file" -> StringValue(s"/DIR/$name"))

  private def externalOrderArised(name: String) =
    aOrderWatch.path <-: ExternalOrderArised(
      ExternalOrderName(name),
      orderId(name),
      NamedValues("file" -> StringValue(s"/DIR/$name")))

  private def externalOrderVanished(name: String) =
    aOrderWatch.path <-: ExternalOrderVanished(ExternalOrderName(name))

  private def externalOrderKey(name: String) =
    ExternalOrderKey(aOrderWatch.path, ExternalOrderName(name))

  private def orderId(name: String) =
    OrderId(s"file:A-SOURCE:$name")

  private def orderAdded(name: String) =
    orderId(name) <-:
      OrderAdded(workflowId,Map("file" -> StringValue(s"/DIR/$name")),
        externalOrderKey = Some(ExternalOrderKey(aOrderWatch.path, ExternalOrderName(name))))

  private def order(name: String): Order[Order.Fresh] = {
    val KeyedEvent(orderId, event) = orderAdded(name)
    Order.fromOrderAdded(orderId, event)
  }
}
