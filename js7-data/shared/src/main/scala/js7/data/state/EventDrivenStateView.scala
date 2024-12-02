package js7.data.state

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.Problems.EventNotHandledHereProblem
import js7.data.board.{BoardItem, BoardState}
import js7.data.event.{Event, EventDrivenState}
import js7.data.item.{UnsignedSimpleItem, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.order.Order.{ExpectingNotices, WaitingForLock}
import js7.data.order.OrderEvent.{OrderAddedX, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderDetached, OrderExternalVanished, OrderForked, OrderJoined, OrderLockEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased, OrderNoticeAnnounced, OrderNoticeEvent, OrderNoticeExpected, OrderNoticePosted, OrderNoticePostedV2_3, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead, OrderOrderAdded, OrderStateReset, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.{ConsumeNotices, LockInstruction}
import js7.data.workflow.position.WorkflowPosition
import scala.reflect.ClassTag

// TODO Replace F-type polymorphism with a typeclass ? https://tpolecat.github.io/2015/04/29/f-bounds.html
trait EventDrivenStateView[Self <: EventDrivenStateView[Self, E], E <: Event]
extends EventDrivenState[Self, E], StateView:
  this: Self =>

  final def isOrderExternalNotVanished(orderId: OrderId): Boolean =
    idToOrder.get(orderId).flatMap(_.externalOrder).exists(o => !o.vanished)

  protected def update(
    addOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[Self] =
    for
      _ <- precheckUpdate(addOrders, removeOrders, externalVanishedOrders,
        addItemStates, removeUnsignedSimpleItems)
      self <- update_(addOrders, removeOrders, externalVanishedOrders,
        addItemStates, removeUnsignedSimpleItems)
    yield
      self

  // Call only via update() !
  protected def update_(
    addOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[Self]

  private def precheckUpdate(
    addOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[Unit] =
    if isStrict then
      for
        _ <- addOrders.view.map(_.id).concat(removeOrders).checkUniqueness
        _ <- externalVanishedOrders.checkUniqueness(_.id)
        _ <- addItemStates.view.map(_.path).concat(removeUnsignedSimpleItems).checkUniqueness
      yield ()
    else
      Checked.unit

  protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[Self] =
    event match
      case _: OrderAddedX | _: OrderEvent.OrderAttachedToAgent =>
        // Event is handled by one of ControllerState and AgentState only
        Left(EventNotHandledHereProblem(event, companion))

      case event: OrderCoreEvent =>
        applyOrderCoreEvent(orderId, event)

      case _: OrderStdWritten =>
        // OrderStdWritten is not applied. But check OrderId.
        idToOrder.checked(orderId).rightAs(this)

  private def applyOrderCoreEvent(orderId: OrderId, event: OrderCoreEvent): Checked[Self] =
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
                Left(Problem:
                  s"For event $event, $orderId must be in Forked state, not: $state")

        case event: OrderLockEvent =>
          event
            .match
              case OrderLocksQueued(demands) =>
                foreachLockDemand(demands):
                  _.enqueue(orderId, _)

              case OrderLocksAcquired(demands) =>
                foreachLockDemand(demands):
                  _.acquire(orderId, _)

              case OrderLocksReleased(lockPaths) =>
                foreachLock(lockPaths):
                  _.release(orderId)
            .flatMap: lockStates =>
              update(
                addOrders = updatedOrder :: Nil,
                addItemStates = lockStates)

        case event: OrderNoticeEvent =>
          applyOrderNoticeEvent(previousOrder, event)
            .flatMap(_.update(addOrders = updatedOrder :: Nil))

        case OrderStateReset =>
          previousOrder.ifState[WaitingForLock].map: order =>
            val instr = instruction_[LockInstruction](order.workflowPosition).orThrow
            foreachLock(instr.lockPaths): lockState =>
              Right:
                lockState.dequeue(orderId)
            .flatMap: lockStates =>
              update(
                addOrders = updatedOrder :: Nil,
                addItemStates = lockStates)
          .orElse:
            previousOrder.ifState[ExpectingNotices].map: order =>
              removeNoticeExpectation(order).flatMap: updatedBoardStates =>
                update(
                  addOrders = updatedOrder :: Nil,
                  addItemStates = updatedBoardStates)
          .getOrElse:
            update(
              addOrders = updatedOrder :: Nil)

        case _: OrderCancelled =>
          previousOrder
            // COMPATIBLE Since v2.7.2 an OrderStateReset is emitted and the
            // following code is superfluous (but still needed for old journals)
            .ifState[ExpectingNotices]
            .fold(update(addOrders = updatedOrder :: Nil)): order =>
              removeNoticeExpectation(order).flatMap: updatedBoardStates =>
                update(
                  addOrders = updatedOrder :: Nil,
                  addItemStates = updatedBoardStates)

        case OrderExternalVanished =>
          if updatedOrder.externalOrder.isEmpty then
            Left(Problem(s"OrderExternalVanished but $orderId is not linked to an external order"))
          else
            update(externalVanishedOrders = updatedOrder :: Nil)

        case OrderDeletionMarked =>
          update(addOrders = updatedOrder :: Nil)

        case OrderDeleted =>
          if isAgent then
            eventNotApplicable(orderId <-: event)
          else
            update(removeOrders = orderId :: Nil)

        case event: OrderOrderAdded =>
          // ControllerState handles this event
          Left(EventNotHandledHereProblem(event, companion))

        case _ =>
          update(addOrders = updatedOrder :: Nil)
    yield
      result

  private def applyOrderNoticeEvent(
    previousOrder: Order[Order.State],
    event: OrderNoticeEvent)
  : Checked[Self] =
    val orderId = previousOrder.id
    event
      .match
        case OrderNoticeAnnounced(boardPath, noticeId) =>
          keyTo(BoardState).checked(boardPath)
            .flatMap:
              _.announceNotice(noticeId)
            .map(_ :: Nil)

        case OrderNoticePostedV2_3(notice) =>
          orderIdToBoardState(orderId)
            .flatMap: boardState =>
              boardState.addNoticeV2_3(notice)
            .map(_ :: Nil)

        case OrderNoticePosted(notice) =>
          keyTo(BoardState).checked(notice.boardPath)
            .flatMap:
              _.addNotice(notice)
            .map(_ :: Nil)

        case OrderNoticeExpected(noticeId) =>
          orderIdToBoardState(orderId)
            .flatMap:
              _.addExpectation(noticeId, orderId)
            .map(_ :: Nil)

        case OrderNoticesExpected(expectedSeq) =>
          expectedSeq.traverse: expected =>
            keyTo(BoardState).checked(expected.boardPath)
              .flatMap:
                _.addExpectation(expected.noticeId, orderId)

        case OrderNoticesRead =>
          previousOrder.ifState[ExpectingNotices] match
            case None => Right(Nil)
            case Some(previousOrder) => removeNoticeExpectation(previousOrder)

        case OrderNoticesConsumptionStarted(consumptions) =>
          val isConsumption = consumptions.toSet
          val expectedOrConsumptionSeq =
            previousOrder.ifState[ExpectingNotices].fold_(Vector.empty, _.state.expected)
              .concat(consumptions).distinct
          if isStrict then assert(expectedOrConsumptionSeq.areUniqueBy(_.boardPath))
          expectedOrConsumptionSeq.traverse: exp =>
            keyTo(BoardState).checked(exp.boardPath).flatMap: boardState =>
              if isConsumption(exp) then
                boardState.startConsumption(exp.noticeId, orderId)
              else
                boardState.removeExpectation(exp.noticeId, orderId)

        case OrderNoticesConsumed(failed) =>
          val consumeNotices: Checked[ConsumeNotices] =
            if failed then
              findInstructionInCallStack[ConsumeNotices](previousOrder.workflowPosition)
            else
              // When succeeding we are pedantic and expect the ConsumeNotice one level up
              previousOrder.workflowPosition.checkedParent.flatMap(instruction_[ConsumeNotices])
          consumeNotices.flatMap(_
            .referencedBoardPaths.toSeq
            .traverse(keyTo(BoardState).checked)
            .flatMap(_.traverse:
              _.finishConsumption(previousOrder.id, succeeded = !failed)))
      .flatMap: o =>
        update(addItemStates = o)

  private def findInstructionInCallStack[I <: Instruction: ClassTag](
    workflowPosition: WorkflowPosition)
  : Checked[I] =
    for
      pos <- workflowPosition.checkedParent
      instr <- instruction_[I](pos).orElse(findInstructionInCallStack[I](pos))
    yield
      instr

  protected final def checkChangedItem(item: UnsignedSimpleItem): Checked[Unit] =
    item match
      case board: BoardItem =>
        keyToUnsignedItemState.get(board.path).fold(Checked.unit): existing =>
          (board.getClass eq existing.item.getClass) !!
            Problem.pure("Type of NoticeBoard cannot be changed")
      case _ =>
        Checked.unit

  private def removeNoticeExpectation(order: Order[ExpectingNotices]): Checked[Seq[BoardState]] =
    order.state.expected
      .traverse: expected =>
        keyTo(BoardState).checked(expected.boardPath).flatMap:
          _.removeExpectation(expected.noticeId, order.id)
