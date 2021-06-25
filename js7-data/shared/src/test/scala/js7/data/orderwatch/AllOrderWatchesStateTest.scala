package js7.data.orderwatch

import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderDeletionMarked}
import js7.data.order.{FreshOrder, Order, OrderId}
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
  private var aows = AllOrderWatchesState.empty

  private def state(name: String): Option[ArisedOrHasOrder] =
    aows.pathToOrderWatchState(aOrderWatch.path).externalToState.get(ExternalOrderName(name))

  "addOrderWatch" in {
    aows = aows.addOrderWatch(aOrderWatch).orThrow
    aows = aows.addOrderWatch(bOrderWatch).orThrow
    assert(aows.addOrderWatch(aOrderWatch) ==
      Left(DuplicateKey("OrderWatchPath", "OrderWatch:A-WATCH")))
  }

  "removeOrderWatch" in {
    val x = bOrderWatch.copy(path = OrderWatchPath("X"))
    val all1 = aows.addOrderWatch(x).orThrow
    assert(all1.pathToOrderWatchState.contains(x.path))
    val all0 = all1.removeOrderWatch(x.path)
    assert(all0 == aows)
  }

  "changeOrderWatch" in {
    val a1 = aOrderWatch.copy(directory = "CHANGED")
    val all1 = aows.changeOrderWatch(a1).orThrow
    assert(all1.pathToOrderWatchState(a1.path) == OrderWatchState(a1))
  }

  "Events on the happy path" - {
    "ExternalOrderArised A, B (and X) --> OrderAdded" in {
      val events = Seq(externalOrderArised("A"), externalOrderArised("X"), externalOrderArised("B"), externalOrderVanished("X"))
      for (event <- events) aows = aows.onOrderWatchEvent(event).orThrow
      assert(state("A") == Some(Arised(orderId("A"), arguments("A"))))
      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("A"), orderAdded("B")))
    }

    "ExternalOrderVanished A, cancels previous Arised if OrderAdded was not emitted" in {
      aows = aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(state("A") == None)
      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("B")))
    }

    "ExternalOrderArised A" in {
      aows = aows.onOrderWatchEvent(externalOrderArised("A")).orThrow
      assert(aows
        .pathToOrderWatchState(aOrderWatch.path)
        .externalToState(ExternalOrderName("A")) == Arised(orderId("A"), arguments("A")))
      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("B"), orderAdded("A")))
    }

    "OrderAdded A, B" in {
      aows = aows.onOrderAdded(orderAdded("A")).orThrow
      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("B")))

      aows = aows.onOrderAdded(orderAdded("B")).orThrow
      assert(aows.nextEvents(toOrderAdded).isEmpty)
    }

    "ExternalOrderVanished A => OrderRemovedMarked" in {
      aows = aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderId("A") <-: OrderDeletionMarked))
    }

    "OrderDeletionMarked A" in {
      aows = aows.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeletionMarked).orThrow
      assert(aows.nextEvents(toOrderAdded).isEmpty)
    }

    "OrderDeleted A" in {
      aows = aows.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeleted).orThrow
      assert(aows.nextEvents(toOrderAdded).isEmpty)
    }

    "ExternalOrderVanished B => OrderRemovedMarked" in {
      aows = aows.onOrderWatchEvent(externalOrderVanished("B")).orThrow
      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderId("B") <-: OrderDeletionMarked))
    }

    "OrderDeletionMarked B" in {
      aows = aows.onOrderEvent(externalOrderKey("B"), orderId("B") <-: OrderDeletionMarked).orThrow
      assert(aows.nextEvents(toOrderAdded).isEmpty)
    }

    "ExternalOrderArised B, while B order is running" in {
      aows = aows.onOrderWatchEvent(externalOrderArised("B")).orThrow
      assert(aows.nextEvents(toOrderAdded).isEmpty)
    }

    "OrderDeleted B => OrderAdded" in {
      aows = aows.onOrderEvent(externalOrderKey("B"), orderId("B") <-: OrderDeleted).orThrow
      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("B")))
      aows = aows.onOrderAdded(orderAdded("B")).orThrow
    }

    "OrderVanished, OrderArised, OrderVanished, OrderArised while order is running" in {
      aows = aows.onOrderWatchEvent(externalOrderVanished("B")).orThrow
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))

      aows = aows.onOrderWatchEvent(externalOrderArised("B")).orThrow
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Arised(orderId("B"), arguments("B"))))))

      aows = aows.onOrderWatchEvent(externalOrderVanished("B")).orThrow
      assert(state("B") == Some(HasOrder(orderId("B"), None)))

      assert(aows.nextEvents(toOrderAdded).toSeq == Seq(orderId("B") <-: OrderDeletionMarked))

      aows = aows.onOrderWatchEvent(externalOrderArised("B")).orThrow
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
    assert(a.nextEvents(toOrderAdded).isEmpty)

    a = a.onOrderWatchEvent(externalOrderArised("C")).orThrow
    assert(a.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("C")))
  }

  "Events in illegal order" - {
    "Double ExternalOrderArised" in {
      aows = aows.onOrderWatchEvent(externalOrderArised("A")).orThrow
      assert(aows.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        """Duplicate ExternalOrderArised(A, Map(file -> '/DIR/A')): Arised(Order:file:A-SOURCE:A,Map(file -> '/DIR/A'))""")))
    }

    "Double early ExternalOrderVanished" in {
      aows = aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(aows.onOrderWatchEvent(externalOrderVanished("A")) == Left(
        UnknownKeyProblem("ExternalOrderName", "A")))
    }

    "Arised after OrderAdded" in {
      aows = aows.onOrderWatchEvent(externalOrderArised("A")).orThrow
      aows = aows.onOrderAdded(orderAdded("A")).orThrow

      // Second ExternalOrderArised is allowed here only because the state is same as after a second ExternalOrderVanished.
      // Could be improved but should not happen, anyway.
      aows.onOrderWatchEvent(externalOrderArised("A")).orThrow
      //assert(aows.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
      //  """Duplicate ExternalOrderArised(A, Map(file -> "/DIR/A")): HasOrder(Order:file:A-SOURCE:A,None)""")))
    }

    "Vanished before OrderDeletionMarked" in {
      aows = aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow
      assert(aows.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-SOURCE:A,Some(Vanished))""")))
      val a = aows.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeletionMarked).orThrow

      assert(aows.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-SOURCE:A,Some(Vanished))""")))
    }

    "Arised without OrderDeletionMarked" in {
      // May happen if OrderDeletionMarked was already emitted
      // - due to explicit command
      // - due to repeated ExternalOrderArised and ExternalOrderVanished
      aows = aows.onOrderWatchEvent(externalOrderArised("A")).orThrow
      assert(aows.pathToOrderWatchState(aOrderWatch.path).externalToState(ExternalOrderName("A")) ==
        HasOrder(orderId("A"), Some(Arised(orderId("A"), order("A").arguments))))

      assert(aows.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderDeletionMarked) == Right(aows))
    }
  }
  private def toOrderAdded(order: FreshOrder, externalOrderKey: Option[ExternalOrderKey] = None)
  : Checked[Option[KeyedEvent[OrderAdded]]] =
    Right(Some(order.toOrderAdded(v1, order.arguments, externalOrderKey)))

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
