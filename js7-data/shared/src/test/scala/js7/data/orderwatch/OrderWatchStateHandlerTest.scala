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
import js7.data.orderwatch.OrderWatchStateHandlerTest.TestState
import js7.data.value.expression.FastparseExpressionParser.expr
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

final class OrderWatchStateHandlerTest extends AnyFreeSpec
{
  private val v1 = VersionId("1")
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val workflowId = workflowPath ~ v1
  private val aOrderWatch = FileWatch(OrderWatchPath("A-WATCH"),
    workflowPath, AgentPath("AGENT"), expr("'DIRECTORY'"))
  private val bOrderWatch = aOrderWatch.copy(path = OrderWatchPath("B-WATCH"))
  private val bothOrders = Set(orderId("A"), orderId("B"))
  private var state = TestState(Map.empty)

  private def state(name: String): Option[ArisedOrHasOrder] =
    state.pathToOrderWatchState(aOrderWatch.path).externalToState.get(ExternalOrderName(name))

  private def update(o: TestState) = {
    state = o
    assert(state.ow.finishRecovery == Right(state))
  }

  "addOrderWatch" in {
    update(state.ow.addOrderWatch(aOrderWatch.toInitialItemState).orThrow)
    update(state.ow.addOrderWatch(bOrderWatch.toInitialItemState).orThrow)
    assert(state.ow.addOrderWatch(aOrderWatch.toInitialItemState) ==
      Left(DuplicateKey("OrderWatchPath", "OrderWatch:A-WATCH")))
  }

  "removeOrderWatch" in {
    val x = bOrderWatch.copy(path = OrderWatchPath("X"))
    val all1 = state.ow.addOrderWatch(x.toInitialItemState).orThrow
    assert(all1.pathToOrderWatchStateMap.contains(x.path))
    val all0 = all1.ow.removeOrderWatch(x.path).orThrow
    assert(all0 == state)
  }

  "changeOrderWatch" in {
    val a1 = aOrderWatch.copy(directory = expr("'CHANGED'"))
    val all1 = state.ow.changeOrderWatch(a1).orThrow
    assert(all1.pathToOrderWatchStateMap(a1.path) == OrderWatchState(a1))
  }

  "Events on the happy path" - {
    "X arises and vanishes before an order could be added" in {
      update(state.ow.onOrderWatchEvent(externalOrderArised("X")).orThrow)
      assert(state("X") == Some(arised("X")))

      update(state.ow.onOrderWatchEvent(externalOrderVanished("X")).orThrow)
      assert(state("X") == None)

      // Initial state
      assert(state == TestState(Map(
        aOrderWatch.path -> OrderWatchState(aOrderWatch),
        bOrderWatch.path -> OrderWatchState(bOrderWatch))))
    }

    "ExternalOrderArised A and B --> OrderAdded" in {
      update(state.ow.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)

      assert(state("A") == Some(arised("A")))
      assert(state("B") == Some(arised("B")))

      assert(state.pathToOrderWatchStateMap == Map(
        aOrderWatch.path -> OrderWatchState(
          aOrderWatch,
          Map.empty,
          Map(
            ExternalOrderName("A") -> arised("A"),
            ExternalOrderName("B") -> arised("B"))),
        bOrderWatch.path -> OrderWatchState((bOrderWatch))))

      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("A"), orderAdded("B")))
    }

    "ExternalOrderVanished cancels previous Arised if OrderAdded was not emitted" in {
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == None)
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B")))
    }

    "ExternalOrderArised A, again" in {
      update(state.ow.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      assert(state
        .pathToOrderWatchState(aOrderWatch.path)
        .externalToState(ExternalOrderName("A")) == arised("A"))
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B"), orderAdded("A")))

      assert(state("A") == Some(arised("A")))
      assert(state("B") == Some(arised("B")))
    }

    "OrderAdded A and B" in {
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B"), orderAdded("A")))

      update(state.ow.onOrderAdded(toOrder("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"))))
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B")))

      update(state.ow.onOrderAdded(toOrder("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"))))
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).isEmpty)
    }

    "ExternalOrderVanished A => OrderDeletionMarked" in {
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"), Some(Vanished))))

      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("A") <-: OrderDeletionMarked))

      update(state.ow.onOrderDeleted(externalOrderKey("A"), orderId("A")).orThrow)
      assert(state("A") == None)

      assert(state.ow.nextEvents(toOrderAdded, bothOrders).isEmpty)
    }

    "ExternalOrderVanished B => OrderDeletionMarked" in {
      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))

      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("B") <-: OrderDeletionMarked))
    }

    "ExternalOrderArised B, while B order is running" in {
      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state.ow.nextEvents(toOrderAdded, Set.empty).isEmpty)
    }

    "OrderDeleted B => OrderAdded" in {
      update(state.ow.onOrderDeleted(externalOrderKey("B"), orderId("B")).orThrow)
      // Now, the queued Arised is in effect
      assert(state("B") == Some(arised("B")))
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("B")))

      update(state.ow.onOrderAdded(toOrder("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"))))
    }

    "OrderVanished, OrderArised, OrderVanished, OrderArised while order is running" in {
      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("B") <-: OrderDeletionMarked))

      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(arised("B")))))

      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(state.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(
        orderId("B") <-: OrderDeletionMarked))

      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(arised("B")))))
    }

    "OrderDeleted" in {
      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      update(state.ow.onOrderDeleted(
        ExternalOrderKey(aOrderWatch.path, ExternalOrderName("B")),
        orderId("B")).orThrow)
      assert(state("A") == None)
      assert(state("B") == None)
    }
  }

  "OrderDeletionMarked (by user) when not Vanished" in {
    var a = TestState(Map.empty)
    a = a.ow.addOrderWatch(aOrderWatch.toInitialItemState).orThrow
    a = a.ow.onOrderWatchEvent(externalOrderArised("C")).orThrow
    a = a.ow.onOrderAdded(toOrder("C")).orThrow

    a = a.ow.onOrderDeleted(externalOrderKey("C"), orderId("C")).orThrow
    assert(a.ow.nextEvents(toOrderAdded, bothOrders).isEmpty)

    a = a.ow.onOrderWatchEvent(externalOrderArised("C")).orThrow
    assert(a.ow.nextEvents(toOrderAdded, bothOrders).toSeq == Seq(orderAdded("C")))
  }

  "Events in illegal order" - {
    "Double ExternalOrderArised" in {
      assert(state("A") == None)
      assert(state("B") == None)

      update(state.ow.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      assert(state.ow.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        """Duplicate ExternalOrderArised(A, Map(file -> '/DIR/A')): Arised(Order:file:A-SOURCE:A,Map(file -> '/DIR/A'))""")))
    }

    "Double early ExternalOrderVanished fails" in {
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state.ow.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        "OrderWatch:A-WATCH: Ignored ExternalOrderVanished(A) event for unknown name")))
    }

    "Arised after OrderAdded" in {
      update(state.ow.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      update(state.ow.onOrderAdded(toOrder("A")).orThrow)

      assert(state.ow.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        "Duplicate ExternalOrderArised(A, Map(file -> '/DIR/A')): HasOrder(Order:file:A-SOURCE:A,None)")))
    }

    "Vanished before OrderDeletionMarked" in {
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"), Some(Vanished))))
      assert(state.ow.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-SOURCE:A,Some(Vanished))""")))

      update(state.ow.onOrderDeleted(externalOrderKey("A"), orderId("A")).orThrow)
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

  private def toOrder(name: String) =
    Order(
      orderId(name),
      workflowId,
      Order.Ready,
      Map("file" -> StringValue(s"/DIR/$name")),
        externalOrderKey = Some(ExternalOrderKey(aOrderWatch.path, ExternalOrderName(name))))
}

object OrderWatchStateHandlerTest
{
  private final case class TestState(pathToOrderWatchStateMap: Map[OrderWatchPath, OrderWatchState])
  extends OrderWatchStateHandler[TestState]
  {
    def pathToOrderWatchState = pathToOrderWatchStateMap.view

    protected def updateOrderWatchStates(
      orderWatchStates: Iterable[OrderWatchState],
      remove: Iterable[OrderWatchPath]) =
      Right(copy(
        pathToOrderWatchStateMap -- remove ++ orderWatchStates.map(o => o.path -> o)))
  }
}
