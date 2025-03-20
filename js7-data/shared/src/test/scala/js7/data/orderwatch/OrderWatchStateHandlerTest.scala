package js7.data.orderwatch

import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.order.Order.ExternalOrderLink
import js7.data.order.OrderEvent.{OrderAdded, OrderAddedEvent, OrderAddedEvents, OrderExternalVanished}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderAppeared, ExternalOrderRejected, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.{Appeared, AppearedOrHasOrder, HasOrder, Vanished}
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

  private def state(name: String): Option[AppearedOrHasOrder] =
    state.pathToOrderWatchState(aOrderWatch.path).externalToState.get(ExternalOrderName(name))

  private def applyNextEvents()
  : Seq[KeyedEvent[OrderAddedEvent | OrderExternalVanished | ExternalOrderRejected]] =
    val events = state.ow.nextEvents(toOrderAdded).toSeq
    events foreach:
      case KeyedEvent(orderId: OrderId, _: OrderAdded) =>
        update:
          state.ow.onOrderAdded(aExternalOrderKey(orderId.string.stripPrefix("file:A-WATCH:")), orderId).orThrow
            .copy(isExternalNotVanished = state.isExternalNotVanished + orderId)
      case KeyedEvent(orderId: OrderId, OrderExternalVanished) =>
        update:
          state.ow.onOrderExternalVanished(aExternalOrderKey(orderId.string.stripPrefix("file:A-WATCH:"))).orThrow
            .copy(isExternalNotVanished = state.isExternalNotVanished - orderId)
      case KeyedEvent(externalOrderName, ExternalOrderRejected) =>
        throw new NotImplementedError
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
    "X appears and vanishes before an order could be added" in:
      update(state.ow.onOrderWatchEvent(externalOrderAppeared("X")).orThrow)
      assert(state("X") == Some(appeared("X")))

      update(state.ow.onOrderWatchEvent(externalOrderVanished("X")).orThrow)
      assert(state("X") == None)

      // We have the initial state
      assert(state == TestState(
        Map(
          aOrderWatch.path -> OrderWatchState(aOrderWatch),
          bOrderWatch.path -> OrderWatchState(bOrderWatch)),
        Set.empty))

    "ExternalOrderAppeared A and B --> OrderAdded" in:
      update(state.ow.onOrderWatchEvent(externalOrderAppeared("A")).orThrow)
      update(state.ow.onOrderWatchEvent(externalOrderAppeared("B")).orThrow)

      assert(state("A") == Some(appeared("A")))
      assert(state("B") == Some(appeared("B")))

      assert(state == TestState(
        Map(
          aOrderWatch.path ->
            OrderWatchState(
              aOrderWatch,
              Map(
                ExternalOrderName("A") -> appeared("A"),
                ExternalOrderName("B") -> appeared("B")),
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
          OrderId("file:A-WATCH:A"),
          OrderId("file:A-WATCH:B"))))

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

    "ExternalOrderAppeared B, while B order is running" in:
      update(state.ow.onOrderWatchEvent(externalOrderAppeared("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(appeared("B")))))
      assert(applyNextEvents().isEmpty)

    "OrderDeleted B => OrderAdded" in:
      update(state.ow.onOrderDeleted(aExternalOrderKey("B"), orderId("B")).orThrow)
      // Now, the queued Appeared is in effect
      assert(state("B") == Some(appeared("B")))

      assert(applyNextEvents() == Seq(orderAdded("B")))
      assert(state("B") == Some(HasOrder(orderId("B"))))

    "ExternalOrderVanished while order is running" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(applyNextEvents() == Seq(
        orderId("B") <-: OrderExternalVanished))

      update(state.ow.onOrderWatchEvent(externalOrderAppeared("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(appeared("B")))))

      update(state.ow.onOrderWatchEvent(externalOrderVanished("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(Vanished))))
      assert(applyNextEvents().isEmpty)
      //assert(applyNextEvents() == Seq(
      //  orderId("B") <-: OrderExternalVanished))

      update(state.ow.onOrderWatchEvent(externalOrderAppeared("B")).orThrow)
      assert(state("B") == Some(HasOrder(orderId("B"), Some(appeared("B")))))

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
    a = a.ow.onOrderWatchEvent(externalOrderAppeared("C")).orThrow
    assert(a.ow.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("C")))

    a = a.ow.onOrderDeleted(aExternalOrderKey("C"), orderId("C")).orThrow
    /// A new Order with same OrderId is immediately added ///
    assert(a.ow.nextEvents(toOrderAdded).toSeq == Seq(orderAdded("C")))

  "Events in illegal order" - {
    "Double ExternalOrderAppeared" in:
      assert(state("A") == None)
      assert(state("B") == None)

      update(state.ow.onOrderWatchEvent(externalOrderAppeared("A")).orThrow)
      assert(state.ow.onOrderWatchEvent(externalOrderAppeared("A")) == Left(Problem(
        """Duplicate ExternalOrderAppeared(A, Map(file -> '/DIR/A')): Appeared""")))

    "Double early ExternalOrderVanished fails" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state.ow.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        "OrderWatch:A-WATCH: Ignored ExternalOrderVanished(A) event for unknown name")))

    "Appeared after OrderAdded" in:
      update(state.ow.onOrderWatchEvent(externalOrderAppeared("A")).orThrow)
      assert(applyNextEvents() == Seq(orderAdded("A")))

      assert(state.ow.onOrderWatchEvent(externalOrderAppeared("A")) == Left(Problem(
        "Duplicate ExternalOrderAppeared(A, Map(file -> '/DIR/A')): HasOrder(Order:file:A-WATCH:A,None)")))

    "Vanished before OrderDeleted" in:
      update(state.ow.onOrderWatchEvent(externalOrderVanished("A")).orThrow)
      assert(state("A") == Some(HasOrder(orderId("A"), Some(Vanished))))

      assert(state.ow.onOrderWatchEvent(externalOrderVanished("A")) == Left(Problem(
        """Duplicate ExternalOrderVanished(A), state=HasOrder(Order:file:A-WATCH:A,Some(Vanished))""")))

      update(state.ow.onOrderDeleted(aExternalOrderKey("A"), orderId("A")).orThrow)
      assert(state("A") == None)
  }

  private def toOrderAdded(order: FreshOrder, externalOrderKey: Option[ExternalOrderKey] = None)
  : Checked[Either[Order[Order.State], OrderAddedEvents]] =
    Right(Right:
      OrderAddedEvents(
        order.toOrderAdded(v1, order.arguments, externalOrderKey),
        Nil))

  private def appeared(name: String) =
    Appeared(NamedValues("file" -> StringValue(s"/DIR/$name")))

  private def externalOrderAppeared(name: String) =
    aOrderWatch.path <-: ExternalOrderAppeared(
      ExternalOrderName(name),
      NamedValues("file" -> StringValue(s"/DIR/$name")))

  private def externalOrderVanished(name: String) =
    aOrderWatch.path <-: ExternalOrderVanished(ExternalOrderName(name))

  private def aExternalOrderKey(name: String) =
    aOrderWatch.path / ExternalOrderName(name)

  private def bExternalOrderKey(name: String) =
    bOrderWatch.path / ExternalOrderName(name)

  private def orderId(name: String) =
    OrderId(s"file:${aOrderWatch.path.string}:$name")

  private def orderAdded(name: String) =
    orderId(name) <-:
      OrderAdded(workflowId,Map("file" -> StringValue(s"/DIR/$name")),
        externalOrderKey = Some(aOrderWatch.path / ExternalOrderName(name)))

  private def toOrder(name: String) =
    Order(
      orderId(name),
      workflowId /: Position(0),
      Order.Ready(),
      arguments = Map("file" -> StringValue(s"/DIR/$name")),
        externalOrder = Some:
          ExternalOrderLink(aOrderWatch.path, ExternalOrderName(name)))


object OrderWatchStateHandlerTest:

  private final case class TestState(
    pathToOrderWatchStateMap: Map[OrderWatchPath, OrderWatchState],
    isExternalNotVanished: Set[OrderId])
  extends OrderWatchStateHandler[TestState]:

    def pathToOrderWatchState = pathToOrderWatchStateMap.view

    protected def updateOrderWatchStates(
      orderWatchStates: Seq[OrderWatchState],
      remove: Seq[OrderWatchPath]) =
      Right(copy(
        pathToOrderWatchStateMap -- remove ++ orderWatchStates.map(o => o.path -> o)))

    def isOrderExternalNotVanished(orderId: OrderId) =
      isExternalNotVanished(orderId)

    protected def idToOrder: PartialFunction[OrderId, Order[Order.State]] =
      throw NotImplementedError()
