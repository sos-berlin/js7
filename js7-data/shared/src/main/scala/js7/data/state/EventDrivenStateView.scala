package js7.data.state

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.BoardState
import js7.data.event.{Event, EventDrivenState}
import js7.data.item.{UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderDetached, OrderForked, OrderJoined, OrderLockEvent, OrderLocksAcquired, OrderLocksDequeued, OrderLocksQueued, OrderLocksReleased, OrderNoticeEvent, OrderNoticeExpected, OrderNoticePosted, OrderNoticePostedV2_3, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead, OrderOrderAdded, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.instructions.ConsumeNotices

// TODO Replace F-type polymorphism with a typeclass ? https://tpolecat.github.io/2015/04/29/f-bounds.html
trait EventDrivenStateView[Self <: EventDrivenStateView[Self, E], E <: Event]
extends EventDrivenState[Self, E], StateView:
  this: Self =>

  protected def update(
    addOrders: Iterable[Order[Order.State]] = Nil,
    removeOrders: Iterable[OrderId] = Nil,
    addItemStates: Iterable[UnsignedSimpleItemState] = Nil,
    removeItemStates: Iterable[UnsignedSimpleItemPath] = Nil)
  : Checked[Self]

  protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case orderAdded: OrderAdded =>
        addOrder(Order.fromOrderAdded(orderId, orderAdded))

      case event: OrderEvent.OrderAttachedToAgent =>
        if idToOrder isDefinedAt orderId then
          Left(Problem.pure(s"Duplicate order attached: $orderId"))
        else
          update(addOrders = Order.fromOrderAttached(orderId, event) :: Nil)

      case event: OrderCoreEvent =>
        applyOrderCoreEvent(orderId, event)

      case _: OrderStdWritten =>
        // OrderStdWritten is not applied. But check OrderId.
        idToOrder.checked(orderId).rightAs(this)

  private def applyOrderCoreEvent(orderId: OrderId, event: OrderCoreEvent): Either[Problem, Self] =
    for
      previousOrder <- idToOrder.checked(orderId)
      updatedOrder <- previousOrder.applyEvent(event)
      result <- event match
        case OrderDetached =>
          if isAgent then
            update(removeOrders = orderId :: Nil)
          else
            update(addOrders = updatedOrder :: Nil)

        case event: OrderForked =>
          update(
            addOrders = updatedOrder +: previousOrder.newForkedOrders(event))

        case event: OrderJoined =>
          if isAgent then
            eventNotApplicable(orderId <-: event)
          else
            previousOrder.state match
              case forked: Order.Forked =>
                update(
                  addOrders = updatedOrder :: Nil,
                  removeOrders = forked.childOrderIds)

              case state =>
                Left(Problem(
                  s"For event $event, $orderId must be in Forked state, not: $state"))

        case event: OrderLockEvent =>
          event
            .match
              case OrderLocksQueued(demands) =>
                foreachLockDemand(demands)(_
                  .enqueue(orderId, _))

              case OrderLocksAcquired(demands) =>
                foreachLockDemand(demands)(_
                  .acquire(orderId, _))

              case OrderLocksDequeued(lockPaths) =>
                foreachLock(lockPaths)(lockState => Right(lockState
                  .dequeue(orderId)))

              case OrderLocksReleased(lockPaths) =>
                foreachLock(lockPaths)(_
                  .release(orderId))
            .flatMap(lockStates =>
              update(
                addOrders = updatedOrder :: Nil,
                addItemStates = lockStates))

        case event: OrderNoticeEvent =>
          applyOrderNoticeEvent(previousOrder, orderId, event)
            .flatMap(_.update(addOrders = updatedOrder :: Nil))

        case _: OrderCancelled =>
          previousOrder
            .ifState[ExpectingNotices]
            .fold(update(addOrders = updatedOrder :: Nil))(order =>
              removeNoticeExpectation(order)
                .flatMap(updatedBoardStates =>
                  update(
                    addOrders = updatedOrder :: Nil,
                    addItemStates = updatedBoardStates)))

        case orderAdded: OrderOrderAdded =>
          addOrder(Order.fromOrderAdded(orderAdded.orderId, orderAdded))

        case OrderDeletionMarked =>
          update(addOrders = updatedOrder :: Nil)

        case OrderDeleted =>
          if isAgent then
            eventNotApplicable(orderId <-: event)
          else
            deleteOrder(previousOrder)
              .flatMap(_.update(removeOrders = orderId :: Nil))

        case _ =>
          update(addOrders = updatedOrder :: Nil)
    yield result

  protected def addOrder(order: Order[Order.State]): Checked[Self] =
    for
      _ <- idToOrder.checkNoDuplicate(order.id)
      self <- update(addOrders = order :: Nil)
    yield self

  protected def deleteOrder(order: Order[Order.State]): Checked[Self] =
    update(removeOrders = order.id :: Nil)

  private def applyOrderNoticeEvent(
    previousOrder: Order[Order.State],
    orderId: OrderId,
    event: OrderNoticeEvent)
  : Checked[Self] =
    event
      .match
        case OrderNoticePostedV2_3(notice) =>
          orderIdToBoardState(orderId)
            .flatMap(boardState => boardState.addNoticeV2_3(notice))
            .map(_ :: Nil)

        case OrderNoticePosted(notice) =>
          keyTo(BoardState)
            .checked(notice.boardPath)
            .flatMap(_.addNotice(notice))
            .map(_ :: Nil)

        case OrderNoticeExpected(noticeId) =>
          orderIdToBoardState(orderId)
            .flatMap(_.addExpectation(noticeId, orderId))
            .map(_ :: Nil)

        case OrderNoticesExpected(expectedSeq) =>
          expectedSeq.traverse(expected =>
            keyTo(BoardState)
              .checked(expected.boardPath)
              .flatMap(_.addExpectation(expected.noticeId, orderId)))

        case OrderNoticesRead =>
          previousOrder.ifState[Order.ExpectingNotices] match
            case None => Right(Nil)
            case Some(previousOrder) => removeNoticeExpectation(previousOrder)

        case OrderNoticesConsumptionStarted(consumptions) =>
          consumptions
            .traverse(consumption =>
              keyTo(BoardState)
                .checked(consumption.boardPath)
                .flatMap(_.addConsumption(consumption.noticeId, previousOrder, consumption)))

        case OrderNoticesConsumed(failed) =>
          previousOrder.workflowPosition.checkedParent
            .flatMap(consumeNoticesPosition =>
              instruction_[ConsumeNotices](consumeNoticesPosition)
                .traverse(_.referencedBoardPaths.toSeq
                  .traverse(keyTo(BoardState).checked))
                .flatten
                .flatMap(_.traverse(_
                  .removeConsumption(previousOrder.id, succeeded = !failed))))
    .flatMap(o => update(
      addItemStates = o))

  private def removeNoticeExpectation(order: Order[Order.State]): Checked[Seq[BoardState]] =
    order.ifState[Order.ExpectingNotices] match
      case None => Right(Nil)
      case Some(order) =>
        order.state.expected
          .traverse(expected => keyTo(BoardState)
            .checked(expected.boardPath)
            .flatMap(_.removeExpectation(expected.noticeId, order.id)))
