package js7.data.orderwatch

import js7.base.problem.Problem
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentId
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderRemoveMarked, OrderRemoved}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.Arised
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import org.scalatest.freespec.AnyFreeSpec

final class AllOrderWatchesStateTest extends AnyFreeSpec
{
  private val v1 = VersionId("1")
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val workflowId = workflowPath ~ v1
  private val aOrderWatch = FileWatch(OrderWatchId("A-SOURCE"),
    workflowPath, AgentId("AGENT"), "DIRECTORY")
  private val bOrderWatch = aOrderWatch.copy(id = OrderWatchId("B-SOURCE"))
  private var aoss = AllOrderWatchesState.empty

  "addOrderWatch" in {
    aoss = aoss.addOrderWatch(aOrderWatch).orThrow
    aoss = aoss.addOrderWatch(bOrderWatch).orThrow
    assert(aoss.addOrderWatch(aOrderWatch) ==
      Left(DuplicateKey("OrderWatchId", "OrderWatch:A-SOURCE")))
  }

  "removeOrderWatch" in {
    val x = bOrderWatch.copy(id = OrderWatchId("X"))
    val all1 = aoss.addOrderWatch(x).orThrow
    assert(all1.idToOrderWatchState.contains(x.id))
    val all0 = all1.removeOrderWatch(x.id)
    assert(all0 == aoss)
  }

  "changeOrderWatch" in {
    val a1 = aOrderWatch.copy(directory = "CHANGED")
    val all1 = aoss.changeOrderWatch(a1).orThrow
    assert(all1.idToOrderWatchState(a1.id) == OrderWatchState(a1))
  }

  "Events on the happy path" - {
    "ExternalOrderArised A, B (and X) --> OrderAdded" in {
      val events = Seq(arised("A"), arised("X"), arised("B"), vanished("X"))
      for (event <- events) aoss = aoss.onOrderWatchEvent(event).orThrow
      assert(aoss
        .idToOrderWatchState(aOrderWatch.id)
        .externalToState(ExternalOrderName("A")) == Arised(orderId("A"), arguments("A")))
      assert(aoss.nextEvents(toVersionId) == Seq(orderAdded("A"), orderAdded("B")))
    }

    "OrderAdded A, B" in {
      aoss = aoss.onOrderAdded(orderAdded("A")).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderAdded("B")))

      aoss = aoss.onOrderAdded(orderAdded("B")).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "ExternalOrderVanished A --> OrderRemovedMarked" in {
      aoss = aoss.onOrderWatchEvent(vanished("A")).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderId("A") <-: OrderRemoveMarked))
    }

    "OrderRemoveMarked A" in {
      aoss = aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderRemoveMarked).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderRemoved A" in {
      aoss = aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderRemoved).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "ExternalOrderVanished B --> OrderRemovedMarked" in {
      aoss = aoss.onOrderWatchEvent(vanished("B")).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderId("B") <-: OrderRemoveMarked))
    }

    "OrderRemoveMarked B" in {
      aoss = aoss.onOrderEvent(externalOrderKey("B"), orderId("B") <-: OrderRemoveMarked).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "ExternalOrderArised B, while B order is running" in {
      aoss = aoss.onOrderWatchEvent(arised("B")).orThrow
      assert(aoss.nextEvents(toVersionId).isEmpty)
    }

    "OrderRemoved B --> OrderAdded" in {
      aoss = aoss.onOrderEvent(externalOrderKey("B"), orderId("B") <-: OrderRemoved).orThrow
      assert(aoss.nextEvents(toVersionId) == Seq(orderAdded("B")))
    }
  }

  "OrderRemoveMarked (by user) when not Vanished" in {
    var a = AllOrderWatchesState.empty
    a = a.addOrderWatch(aOrderWatch).orThrow
    a = a.onOrderWatchEvent(arised("C")).orThrow
    a = a.onOrderAdded(orderAdded("C")).orThrow

    a = a.onOrderEvent(externalOrderKey("C"), orderId("C") <-: OrderRemoveMarked).orThrow
    assert(a.onOrderWatchEvent(vanished("C")) == Right(a))

    a = a.onOrderEvent(externalOrderKey("C"), orderId("C") <-: OrderRemoved).orThrow
    assert(a.nextEvents(toVersionId).isEmpty)

    a = a.onOrderWatchEvent(arised("C")).orThrow
    assert(a.nextEvents(toVersionId) == Seq(orderAdded("C")))
  }

  "Events in illegal order" - {
    "Double ExternalOrderArised" in {
      aoss = aoss.onOrderWatchEvent(arised("A")).orThrow
      assert(aoss.onOrderWatchEvent(arised("A")) == Left(Problem(
        """Duplicate onExternalOrderArised(A, Map(file -> "/DIR/A"))""")))
    }

    "Double early ExternalOrderVanished" in {
      aoss = aoss.onOrderWatchEvent(vanished("A")).orThrow
      assert(aoss.onOrderWatchEvent(vanished("A")) == Left(
        UnknownKeyProblem("ExternalOrderName", "A")))
    }

    "Arised after OrderAdded" in {
      aoss = aoss.onOrderWatchEvent(arised("A")).orThrow
      aoss = aoss.onOrderAdded(orderAdded("A")).orThrow
      assert(aoss.onOrderWatchEvent(arised("A")) == Left(Problem(
        """Duplicate onExternalOrderArised(A, Map(file -> "/DIR/A"))""")))
    }

    "Vanished before OrderRemoveMarked" in {
      aoss = aoss.onOrderWatchEvent(vanished("A")).orThrow
      assert(aoss.onOrderWatchEvent(vanished("A")) == Left(Problem(
        """onExternalOrderVanished(A) but not Arised""")))
      val a = aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderRemoveMarked).orThrow
      // Vanished ignored because it may come from RemoveOrdersWhenTerminated command
      assert(a.onOrderWatchEvent(vanished("A")) == Right(a))
      assert(a.onOrderWatchEvent(vanished("A")) == Right(a))
    }

    "Arised before OrderRemoveMarked" in {
      assert(aoss.onOrderWatchEvent(arised("A")) == Left(Problem(
        """onExternalOrderArised(A, Map(file -> "/DIR/A")) before OrderRemoveMarked""")))
      aoss = aoss.onOrderEvent(externalOrderKey("A"), orderId("A") <-: OrderRemoveMarked).orThrow
      aoss = aoss.onOrderWatchEvent(arised("A")).orThrow
    }
  }

  private def toVersionId(workflowPath: WorkflowPath) = Some(v1)

  private def arguments(name: String) =
    NamedValues("file" -> StringValue(s"/DIR/$name"))

  private def arised(name: String) =
    aOrderWatch.id <-: ExternalOrderArised(
      ExternalOrderName(name),
      orderId(name),
      NamedValues("file" -> StringValue(s"/DIR/$name")))

  private def vanished(name: String) =
    aOrderWatch.id <-: ExternalOrderVanished(ExternalOrderName(name))

  private def externalOrderKey(name: String) =
    ExternalOrderKey(aOrderWatch.id, ExternalOrderName(name))

  private def orderId(name: String) =
    OrderId(s"file:A-SOURCE:$name")

  private def orderAdded(name: String) =
    orderId(name) <-:
      OrderAdded(workflowId,Map("file" -> StringValue(s"/DIR/$name")),
        externalOrderKey = Some(ExternalOrderKey(aOrderWatch.id, ExternalOrderName(name))))
}
