package js7.data.orderwatch

import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.Order.ExternalOrderLink
import js7.data.order.OrderEvent.{OrderAdded, OrderExternalVanished}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.{Arised, ArisedOrHasOrder, HasOrder, Vanished}
import js7.data.orderwatch.OrderWatchStateHandlerTest.TestState
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position

final class OrderWatchStateHandlerTest extends OurTestSuite:

  private val v1 = VersionId("1")
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val workflowId = workflowPath ~ v1
  private val aOrderWatch = FileWatch(OrderWatchPath("A-WATCH"),
    workflowPath, AgentPath("AGENT"), expr("'DIRECTORY'"))
  private val bOrderWatch = aOrderWatch.copy(path = OrderWatchPath("B-WATCH"))
  private var state = TestState(Map.empty, Set.empty)

  private def state(name: String): Option[ArisedOrHasOrder] =
    state.pathToOrderWatchState(aOrderWatch.path).externalToState.get(ExternalOrderName(name))

  private def applyNextEvents(): Seq[KeyedEvent[OrderAdded | OrderExternalVanished]] =
    val events = state.ow.nextEvents(toOrderAdded).toSeq
    events foreach:
      case KeyedEvent(orderId, _: OrderAdded) =>
        update:
          state.ow.onOrderAdded(aExternalOrderKey(orderId.string.stripPrefix("file:A-SOURCE:")), orderId).orThrow
            .copy(isExternalNotVanished = state.isExternalNotVanished + orderId)
      case KeyedEvent(orderId, OrderExternalVanished) =>
        update:
          state.ow.onOrderExternalVanished(aExternalOrderKey(orderId.string.stripPrefix("file:A-SOURCE:"))).orThrow
            .copy(isExternalNotVanished = state.isExternalNotVanished - orderId)
    events

  private def update(o: TestState) =
    state = o
    assert(state.ow.finishRecovery == Right(state))

  "addOrderWatch" in:
    update(state.ow.addOrderWatch(aOrderWatch.toInitialItemState).orThrow)
    update(state.ow.addOrderWatch(bOrderWatch.toInitialItemState).orThrow)
    assert(state.ow.addOrderWatch(aOrderWatch.toInitialItemState) == Left:
      DuplicateKey("OrderWatchPath", "OrderWatch:A-WATCH"))

  "removeOrderWatch" in:
    val x = bOrderWatch.copy(path = OrderWatchPath("X"))
    val all1 = state.ow.addOrderWatch(x.toInitialItemState).orThrow
    assert(all1.pathToOrderWatchStateMap.contains(x.path))

    val all0 = all1.ow.removeOrderWatch(x.path).orThrow
    assert(all0 == state)

  "changeOrderWatch" in:
    val a1 = aOrderWatch.copy(directoryExpr = expr("'CHANGED'"))
    val all1 = state.ow.changeOrderWatch(a1).orThrow
    assert(all1.pathToOrderWatchStateMap(a1.path) == OrderWatchState(a1))

  "Events on the happy path" - {
    "X arises and vanishes before an order could be added" in:
      update(state.ow.onOrderWatchEvent(externalOrderArised("X")).orThrow)
      assert(state("X") == Some(arised("X")))

      update(state.ow.onOrderWatchEvent(externalOrderVanished("X")).orThrow)
      assert(state("X") == None)

      // We have the initial state
      assert(state == TestState(
        Map(
          aOrderWatch.path -> OrderWatchState(aOrderWatch),
          bOrderWatch.path -> OrderWatchState(bOrderWatch)),
        Set.empty))

    "ExternalOrderArised A and B --> OrderAdded" in:
      update(state.ow.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)

      assert(state("A") == Some(arised("A")))
      assert(state("B") == Some(arised("B")))

      assert(state == TestState(
        Map(
          aOrderWatch.path ->
            OrderWatchState(
              aOrderWatch,
              Map(
                ExternalOrderName("A") -> arised("A"),
                ExternalOrderName("B") -> arised("B")),
              orderAddedQueue = Set(ExternalOrderName("A"), ExternalOrderName("B"))),
          bOrderWatch.path -> OrderWatchState(bOrderWatch)),
        isExternalNotVanished = Set.empty))

      assert(applyNextEvents() == Seq(orderAdded("A"), orderAdded("B")))
      assert(applyNextEvents().isEmpty)

      assert(state == TestState(
        Map(
          aOrderWatch.path ->
            OrderWatchState(
              aOrderWatch,
              Map(
                ExternalOrderName("A") -> HasOrder(orderId("A")),
                ExternalOrderName("B") -> HasOrder(orderId("B")))),
          bOrderWatch.path -> OrderWatchState(bOrderWatch)),
        isExternalNotVanished = Set(
          OrderId("file:A-SOURCE:A"),
          OrderId("file:A-SOURCE:B"))))

    "ExternalOrderVanished A => OrderExternalVanished" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"), Some(Vanished))))

      assert(applyNextEvents() == Seq(
        orderId("A") <-: OrderExternalVanished))

      update(state.ow.onOrderDeleted(aExternalOrderKey("A"), orderId("A")).orThrow)
      assert(state("A") == None)

      assert(applyNextEvents().isEmpty)

    "ExternalOrderVanished B => OrderExternalVanished" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))

      assert(applyNextEvents() == Seq(
        orderId("B") <-: OrderExternalVanished))

    "ExternalOrderArised B, while B order is running" in:
      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(arised("B")))))
      assert(applyNextEvents().isEmpty)

    "OrderDeleted B => OrderAdded" in:
      update(state.ow.onOrderDeleted(aExternalOrderKey("B"), orderId("B")).orThrow)
      // Now, the queued Arised is in effect
      assert(state("B") == Some(arised("B")))

      assert(applyNextEvents() == Seq(orderAdded("B")))
      assert(state("B") == Some(HasOrder(orderId("B"))))

    "OrderVanished, OrderArised, OrderVanished, OrderArised while order is running" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(applyNextEvents() == Seq(
        orderId("B") <-: OrderExternalVanished))

      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(arised("B")))))

      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(applyNextEvents().isEmpty)
      //assert(applyNextEvents() == Seq(
      //  orderId("B") <-: OrderExternalVanished))

      update(state.ow.onOrderWatchEvent(externalOrderArised("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(arised("B")))))

    "OrderExternalVanished" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      update(state.ow
        .onOrderExternalVanished(aExternalOrderKey("B"))
        .orThrow)
      assert(state("A") == None)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))

    "OrderDeleted" in:
      update(state.ow.onOrderDeleted(aExternalOrderKey("B"), orderId("B")).orThrow)
      assert(state("B") == None)
  }

  "OrderDeleted when not Vanished" in:
    var a = TestState(Map.empty, Set.empty)
    a = a.ow.addOrderWatch(aOrderWatch.toInitialItemState).orThrow
    a = a.ow.onOrderWatchEvent(externalOrderArised("C")).orThrow
    assert(a.ow.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("C")))

    a = a.ow.onOrderDeleted(aExternalOrderKey("C"), orderId("C")).orThrow
    /// A new Order with same OrderId is immediately added ///
    assert(a.ow.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("C")))

  "Events in illegal order" - {
    "Double ExternalOrderArised" in:
      assert(state("A") == None)
      assert(state("B") == None)

      update(state.ow.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      assert(state.ow.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        """Duplicate ExternalOrderArised(A, Map(file -> '/DIR/A')): Arised(Order:file:A-SOURCE:A)""")))

    "Double early ExternalOrderVanished fails" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state.ow.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        "OrderWatch:A-WATCH: Ignored ExternalOrderVanished(A) event for unknown name")))

    "Arised after OrderAdded" in:
      update(state.ow.onOrderWatchEvent(externalOrderArised("A")).orThrow)
      assert(applyNextEvents() == Seq(orderAdded("A")))

      assert(state.ow.onOrderWatchEvent(externalOrderArised("A")) == Left(Problem(
        "Duplicate ExternalOrderArised(A, Map(file -> '/DIR/A')): HasOrder(Order:file:A-SOURCE:A,None)")))

    "Vanished before OrderDeleted" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"), Some(Vanished))))

      assert(state.ow.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-SOURCE:A,Some(Vanished))""")))

      update(state.ow.onOrderDeleted(aExternalOrderKey("A"), orderId("A")).orThrow)
      assert(state("A") == None)
  }

  private def toOrderAdded(order: FreshOrder, externalOrderKey: Option[ExternalOrderKey] = None)
  : Checked[Either[Order[Order.State], KeyedEvent[OrderAdded]]] =
    Right(Right:
      order.toOrderAdded(v1, order.arguments, externalOrderKey))

  private def arised(name: String) =
    Arised(orderId(name), NamedValues("file" -> StringValue(s"/DIR/$name")))

  private def externalOrderArised(name: String) =
    aOrderWatch.path <-: ExternalOrderArised(
      ExternalOrderName(name),
      orderId(name),
      NamedValues("file" -> StringValue(s"/DIR/$name")))

  private def externalOrderVanished(name: String) =
    aOrderWatch.path <-: ExternalOrderVanished(ExternalOrderName(name))

  private def aExternalOrderKey(name: String) =
    ExternalOrderKey(aOrderWatch.path, ExternalOrderName(name))

  private def bExternalOrderKey(name: String) =
    ExternalOrderKey(bOrderWatch.path, ExternalOrderName(name))

  private def orderId(name: String) =
    OrderId(s"file:A-SOURCE:$name")

  private def orderAdded(name: String) =
    orderId(name) <-:
      OrderAdded(workflowId,Map("file" -> StringValue(s"/DIR/$name")),
        externalOrderKey = Some(ExternalOrderKey(aOrderWatch.path, ExternalOrderName(name))))

  private def toOrder(name: String) =
    Order(
      orderId(name),
      workflowId /: Position(0),
      Order.Ready,
      Map("file" -> StringValue(s"/DIR/$name")),
        externalOrder = Some:
          ExternalOrderLink(aOrderWatch.path, ExternalOrderName(name)))


object OrderWatchStateHandlerTest:

  private final case class TestState(
    pathToOrderWatchStateMap: Map[OrderWatchPath, OrderWatchState],
    isExternalNotVanished: Set[OrderId])
  extends OrderWatchStateHandler[TestState]:

    def pathToOrderWatchState = pathToOrderWatchStateMap.view

    protected def updateOrderWatchStates(
      orderWatchStates: Iterable[OrderWatchState],
      remove: Iterable[OrderWatchPath]) =
      Right(copy(
        pathToOrderWatchStateMap -- remove ++ orderWatchStates.map(o => o.path -> o)))

    def isOrderExternalNotVanished(orderId: OrderId) =
      isExternalNotVanished(orderId)

    protected def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
      throw NotImplementedError()
