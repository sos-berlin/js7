package js7.data.ordersource

import js7.base.problem.Problem
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentId
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderRemoveMarked, OrderRemoved}
import js7.data.order.OrderId
import js7.data.ordersource.OrderSourceEvent.{OrderSourceOrderArised, OrderSourceOrderVanished}
import js7.data.ordersource.OrderSourceState.Arised
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

final class AllOrderSourcesStateTest extends AnyFreeSpec
{
  private val v1 = VersionId("1")
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val workflowId = workflowPath ~ v1
  private val aOrderSource = FileOrderSource(OrderSourceId("A-SOURCE"),
    workflowPath, AgentId("AGENT"), "DIRECTORY")
  private val bOrderSource = aOrderSource.copy(id = OrderSourceId("B-SOURCE"))
  private var aoss = AllOrderSourcesState.empty

  "addOrderSource" in {
    aoss = aoss.addOrderSource(aOrderSource).orThrow
    aoss = aoss.addOrderSource(bOrderSource).orThrow
    assert(aoss.addOrderSource(aOrderSource) ==
      Left(DuplicateKey("OrderSourceId", "OrderSource:A-SOURCE")))
  }

  "removeOrderSource" in {
    val x = bOrderSource.copy(id = OrderSourceId("X"))
    val all1 = aoss.addOrderSource(x).orThrow
    assert(all1.idToOrderSourceState.contains(x.id))
    val all0 = all1.removeOrderSource(x.id)
    assert(all0 == aoss)
  }

  "changeOrderSource" in {
    val a1 = aOrderSource.copy(directory = "CHANGED")
    val all1 = aoss.changeOrderSource(a1).orThrow
    assert(all1.idToOrderSourceState(a1.id) == OrderSourceState(a1))
  }

  "Events on the happy path" - {
    "OrderSourceOrderArised A, B (and X) --> OrderAdded" in {
      val events = Seq(arised("A"), arised("X"), arised("B"), vanished("X"))
      for (event <- events) aoss = aoss.onOrderSourceEvent(event).orThrow
      assert(aoss
        .idToOrderSourceState(aOrderSource.id)
        .sourceOrderToState(SourceOrderName("A")) == Arised(arguments("A")))
      assert(aoss.nextEvents(toVersionId) == Seq(orderAdded("A"), orderAdded("B")))
    }

    "OrderAdded A, B" in {
      aoss = aoss.onOrderAdded(orderAdded("A")).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderAdded("B")))

      aoss = aoss.onOrderAdded(orderAdded("B")).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderSourceOrderVanished A --> OrderRemovedMarked" in {
      aoss = aoss.onOrderSourceEvent(vanished("A")).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderId("A") <-: OrderRemoveMarked))
    }

    "OrderRemoveMarked A" in {
      aoss = aoss.onOrderEvent(sourceOrderKey("A"), orderId("A") <-: OrderRemoveMarked).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderRemoved A" in {
      aoss = aoss.onOrderEvent(sourceOrderKey("A"), orderId("A") <-: OrderRemoved).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderSourceOrderVanished B --> OrderRemovedMarked" in {
      aoss = aoss.onOrderSourceEvent(vanished("B")).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderId("B") <-: OrderRemoveMarked))
    }

    "OrderRemoveMarked B" in {
      aoss = aoss.onOrderEvent(sourceOrderKey("B"), orderId("B") <-: OrderRemoveMarked).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderSourceOrderArised B, while B order is running" in {
      aoss = aoss.onOrderSourceEvent(arised("B")).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderRemoved B --> OrderAdded" in {
      aoss = aoss.onOrderEvent(sourceOrderKey("B"), orderId("B") <-: OrderRemoved).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderAdded("B")))
    }
  }

  "OrderRemoveMarked (by user) when not Vanished" in {
    var a = AllOrderSourcesState.empty
    a = a.addOrderSource(aOrderSource).orThrow
    a = a.onOrderSourceEvent(arised("C")).orThrow
    a = a.onOrderAdded(orderAdded("C")).orThrow

    a = a.onOrderEvent(sourceOrderKey("C"), orderId("C") <-: OrderRemoveMarked).orThrow
    assert(a.onOrderSourceEvent(vanished("C")) == Right(a))

    a = a.onOrderEvent(sourceOrderKey("C"), orderId("C") <-: OrderRemoved).orThrow
    assert(a.nextEvents(toVersionId).isEmpty)

    a = a.onOrderSourceEvent(arised("C")).orThrow
    assert(a.nextEvents(toVersionId) == Seq(orderAdded("C")))
  }

  "Events in illegal order" - {
    "Double OrderSourceOrderArised" in {
      aoss = aoss.onOrderSourceEvent(arised("A")).orThrow
      assert(aoss.onOrderSourceEvent(arised("A")) == Left(Problem(
        """Duplicate onSourceOrderArised(A, Map(file -> "/DIR/A"))""")))
    }

    "Double early OrderSourceOrderVanished" in {
      aoss = aoss.onOrderSourceEvent(vanished("A")).orThrow
      assert(aoss.onOrderSourceEvent(vanished("A")) == Left(
        UnknownKeyProblem("SourceOrderName", "A")))
    }

    "Arised after OrderAdded" in {
      aoss = aoss.onOrderSourceEvent(arised("A")).orThrow
      aoss = aoss.onOrderAdded(orderAdded("A")).orThrow
      assert(aoss.onOrderSourceEvent(arised("A")) == Left(Problem(
        """Duplicate onSourceOrderArised(A, Map(file -> "/DIR/A"))""")))
    }

    "Vanished before OrderRemoveMarked" in {
      aoss = aoss.onOrderSourceEvent(vanished("A")).orThrow
      assert(aoss.onOrderSourceEvent(vanished("A")) == Left(Problem(
        """onSourceOrderVanished(A) but not Arised""")))
      val a = aoss.onOrderEvent(sourceOrderKey("A"), orderId("A") <-: OrderRemoveMarked).orThrow
      // Vanished ignored because it may come from RemoveOrdersWhenTerminated command
      assert(a.onOrderSourceEvent(vanished("A")) == Right(a))
      assert(a.onOrderSourceEvent(vanished("A")) == Right(a))
    }

    "Arised before OrderRemoveMarked" in {
      assert(aoss.onOrderSourceEvent(arised("A")) == Left(Problem(
        """onSourceOrderArised(A, Map(file -> "/DIR/A")) before OrderRemoveMarked""")))
      aoss = aoss.onOrderEvent(sourceOrderKey("A"), orderId("A") <-: OrderRemoveMarked).orThrow
      aoss = aoss.onOrderSourceEvent(arised("A")).orThrow
    }
  }

  private def toVersionId(workflowPath: WorkflowPath) = Some(v1)

  private def arguments(name: String) =
    NamedValues("file" -> StringValue(s"/DIR/$name"))

  private def arised(name: String) =
    aOrderSource.id <-: OrderSourceOrderArised(
      SourceOrderName(name),
      NamedValues("file" -> StringValue(s"/DIR/$name")))

  private def vanished(name: String) =
    aOrderSource.id <-: OrderSourceOrderVanished(SourceOrderName(name))

  private def sourceOrderKey(name: String) =
    SourceOrderKey(aOrderSource.id, SourceOrderName(name))

  private def orderId(name: String) =
    OrderId(s"FileOrderSource:A-SOURCE:$name")

  private def orderAdded(name: String) =
    orderId(name) <-:
      OrderAdded(workflowId,Map("file" -> StringValue(s"/DIR/$name")),
        sourceOrderKey = Some(SourceOrderKey(aOrderSource.id, SourceOrderName(name))))
}
