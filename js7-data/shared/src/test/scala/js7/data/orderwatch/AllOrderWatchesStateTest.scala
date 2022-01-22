package js7.data.orderwatch

import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderDeletionMarked}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.{Arised, ArisedOrHasOrder, HasOrder, Vanished}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

final class AllOrderWatchesStateTest extends AnyFreeSpec
{
  private val v1 = VersionId("1")
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val workflowId = workflowPath ~ v1
  private val aOrderWatch = FileWatch(OrderWatchPath("A-WATCH"),
    workflowPath, AgentPath("AGENT"), expr("'DIRECTORY'"))
  private val bOrderWatch = aOrderWatch.copy(path = OrderWatchPath("B-WATCH"))
  private val bothOrders = Set(orderId("A"), orderId("B"))
  private var aows = AllOrderWatchesState.empty

  private def state(name: String): Option[ArisedOrHasOrder] =
    aows.pathToOrderWatchState(aOrderWatch.path).externalToState.get(ExternalOrderName(name))

  private def update(o: AllOrderWatchesState) = {
    aows = o
    assert(aows.finishRecovery == Right(aows))
  }

  "addOrderWatch" in {
    update(aows.addOrderWatch(aOrderWatch).orThrow)
    update(aows.addOrderWatch(bOrderWatch).orThrow)
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
    val a1 = aOrderWatch.copy(directory = expr("'CHANGED'"))
    val all1 = aows.changeOrderWatch(a1).orThrow
    assert(all1.pathToOrderWatchState(a1.path) == OrderWatchState(a1))
  }

  "Events on the happy path" - {
    "X arises and vanishes before an order could be added" in {
      update(aows.onOrderWatchEvent(externalOrderArised("X")).orThrow)
      assert(state("X") == Some(arised("X")))

      update(aows.onOrderWatchEvent(externalOrderVanished("X")).orThrow)
      assert(state("X") == None)

      // Initial state
      assert(aows == AllOrderWatchesState(Map(
        aOrderWatch.path -> OrderWatchState(aOrderWatch),
        bOrderWatch.path -> OrderWatchState(bOrderWatch))))
    }

    "ExternalOrderArised A and B --> OrderAdded" in {
      update(aows.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      update(aows.onOrderWatchEvent(externalOrderArised("B")).orThrow)

      assert(state("A") == Some(arised("A")))
      assert(state("B") == Some(arised("B")))

      assert(aows.pathToOrderWatchState == Map(
        aOrderWatch.path -> OrderWatchState(
          aOrderWatch,
          Map.empty,
          Map(
            ExternalOrderName("A") -> arised("A"),
            ExternalOrderName("B") -> arised("B"))),
        bOrderWatch.path -> OrderWatchState((bOrderWatch))))

      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("A"), orderAdded("B")))
    }

    "ExternalOrderVanished cancels previous Arised if OrderAdded was not emitted" in {
      update(aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == None)
      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B")))
    }

    "ExternalOrderArised A, again" in {
      update(aows.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      assert(aows
        .pathToOrderWatchState(aOrderWatch.path)
        .externalToState(ExternalOrderName("A")) == arised("A"))
      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B"), orderAdded("A")))

      assert(state("A") == Some(arised("A")))
      assert(state("B") == Some(arised("B")))
    }

    "OrderAdded A and B" in {
      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B"), orderAdded("A")))

      update(aows.onOrderAdded(orderAdded("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"))))
      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B")))

      update(aows.onOrderAdded(orderAdded("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"))))
      assert(aows.nextEvents(toOrderAdded, bothOrders).isEmpty)
    }

    "ExternalOrderVanished A => OrderDeletionMarked" in {
      update(aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"), Some(Vanished))))

      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("A") <-: OrderDeletionMarked))

      update(aows.onOrderDeleted(externalOrderKey("A"), orderId("A")).orThrow)
      assert(state("A") == None)

      assert(aows.nextEvents(toOrderAdded, bothOrders).isEmpty)
    }

    "ExternalOrderVanished B => OrderDeletionMarked" in {
      update(aows.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))

      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("B") <-: OrderDeletionMarked))
    }

    "ExternalOrderArised B, while B order is running" in {
      update(aows.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(aows.nextEvents(toOrderAdded, Set.empty).isEmpty)
    }

    "OrderDeleted B => OrderAdded" in {
      update(aows.onOrderDeleted(externalOrderKey("B"), orderId("B")).orThrow)
      // Now, the queued Arised is in effect
      assert(state("B") == Some(arised("B")))
      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B")))

      update(aows.onOrderAdded(orderAdded("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"))))
    }

    "OrderVanished, OrderArised, OrderVanished, OrderArised while order is running" in {
      update(aows.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("B") <-: OrderDeletionMarked))

      update(aows.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(arised("B")))))

      update(aows.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(aows.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("B") <-: OrderDeletionMarked))

      update(aows.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(arised("B")))))
    }

    "OrderDeleted" in {
      update(aows.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      update(aows.onOrderDeleted(
        ExternalOrderKey(aOrderWatch.path, ExternalOrderName("B")),
        orderId("B")).orThrow)
      assert(state("A") == None)
      assert(state("B") == None)
    }
  }

  "OrderDeletionMarked (by user) when not Vanished" in {
    var a = AllOrderWatchesState.empty
    a = a.addOrderWatch(aOrderWatch).orThrow
    a = a.onOrderWatchEvent(externalOrderArised("C")).orThrow
    a = a.onOrderAdded(orderAdded("C")).orThrow

    a = a.onOrderDeleted(externalOrderKey("C"), orderId("C")).orThrow
    assert(a.nextEvents(toOrderAdded, bothOrders).isEmpty)

    a = a.onOrderWatchEvent(externalOrderArised("C")).orThrow
    assert(a.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("C")))
  }

  "Events in illegal order" - {
    "Double ExternalOrderArised" in {
      assert(state("A") == None)
      assert(state("B") == None)

      update(aows.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      assert(aows.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        """Duplicate ExternalOrderArised(A, Map(file -> '/DIR/A')): Arised(Order:file:A-SOURCE:A,Map(file -> '/DIR/A'))""")))
    }

    "Double early ExternalOrderVanished fails" in {
      update(aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(aows.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        "OrderWatch:A-WATCH: Ignored ExternalOrderVanished(A) event for unknown name")))
    }

    "Arised after OrderAdded" in {
      update(aows.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      update(aows.onOrderAdded(orderAdded("A")).orThrow)

      assert(aows.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        "Duplicate ExternalOrderArised(A, Map(file -> '/DIR/A')): HasOrder(Order:file:A-SOURCE:A,None)")))
    }

    "Vanished before OrderDeletionMarked" in {
      update(aows.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"), Some(Vanished))))
      assert(aows.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-SOURCE:A,Some(Vanished))""")))

      update(aows.onOrderDeleted(externalOrderKey("A"), orderId("A")).orThrow)
      assert(state("A") == None)
    }
  }

  private def toOrderAdded(order: FreshOrder, externalOrderKey: Option[ExternalOrderKey] = None)
  : Checked[Option[KeyedEvent[OrderAdded]]] =
    Right(Some(order.toOrderAdded(v1, order.arguments, externalOrderKey)))

  private def arised(name: String) =
    Arised(orderId(name), NamedValues("file" -> StringValue(s"/DIR/$name")))

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
