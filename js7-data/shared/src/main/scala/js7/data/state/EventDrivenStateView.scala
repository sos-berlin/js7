package js7.data.state

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.{assertThat, strictly}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.Problems.EventNotHandledHereProblem
import js7.data.board.{BoardState, Notice, NoticeId, PlannedNoticeKey}
import js7.data.event.{Event, EventDrivenState}
import js7.data.item.{UnsignedSimpleItem, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.order.Order.{ExpectingNotices, WaitingForLock}
import js7.data.order.OrderEvent.{OrderAddedX, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderDetached, OrderExternalVanished, OrderForked, OrderJoined, OrderLockEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased, OrderNoticeAnnounced, OrderNoticeEvent, OrderNoticeExpected, OrderNoticePosted, OrderNoticePostedV2_3, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderNoticesRead, OrderOrderAdded, OrderStateReset, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchemaState}
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.{ConsumeNotices, LockInstruction}
import js7.data.workflow.position.WorkflowPosition
import scala.reflect.ClassTag

// TODO Replace F-type polymorphism with a typeclass ? https://tpolecat.github.io/2015/04/29/f-bounds.html
/** The common part of ControllerState and AgentState. */
trait EventDrivenStateView[Self <: EventDrivenStateView[Self]]
extends EventDrivenState[Self, Event], StateView:
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
          applyOrderNoticeEvent(previousOrder, event).flatMap: itemStates =>
            update(
              addOrders = updatedOrder :: Nil,
              addItemStates = itemStates)

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

  private def applyOrderNoticeEvent(order: Order[Order.State], event: OrderNoticeEvent)
  : Checked[Seq[BoardState | PlanSchemaState]] =
    val orderId = order.id
    event match
      case OrderNoticeAnnounced(noticeId) =>
        for
          boardState <- keyTo(BoardState).checked(noticeId.boardPath)
          boardState <- boardState.announceNotice(noticeId.plannedNoticeKey)
          maybePlanSchemaState <- updateNoticeIdInPlan(noticeId, boardState)
        yield
          boardState :: maybePlanSchemaState.toList

      case OrderNoticePostedV2_3(notice) =>
        for
          boardState <- orderIdToBoardState(orderId)
          boardState <- boardState.addNoticeV2_3(notice)
          noticeId = PlanId.Global / boardState.path / notice.noticeKey
          maybePlanSchemaState <- updateNoticeIdInPlan(noticeId, boardState)
        yield
          boardState :: maybePlanSchemaState.toList

      case OrderNoticePosted(noticeId, endOfLife) =>
        for
          boardState <- keyTo(BoardState).checked(noticeId.boardPath)
          boardState <- boardState.addNotice(Notice(noticeId, endOfLife))
          maybePlanSchemaState <- updateNoticeIdInPlan(noticeId, boardState)
        yield
          boardState :: maybePlanSchemaState.toList

      case OrderNoticeExpected(noticeKey) =>
        // COMPATIBLE with v2.3
        for
          boardState <- orderIdToBoardState(orderId)
          noticeId = PlanId.Global / boardState.path / noticeKey
          boardState <- boardState.addExpectation(noticeId.plannedNoticeKey, orderId)
          maybePlanSchemaState <- updateNoticeIdInPlan(noticeId, boardState)
        yield
          boardState :: maybePlanSchemaState.toList

      case OrderNoticesExpected(noticeIds) =>
        noticeIds.traverse: noticeId =>
          for
            boardState <- keyTo(BoardState).checked(noticeId.boardPath)
            boardState <- boardState.addExpectation(noticeId.plannedNoticeKey, orderId)
          yield
            boardState -> noticeId.plannedNoticeKey
        .flatMap: boardStatesAndNoticeIds =>
          updateNoticeIdsInPlans(boardStatesAndNoticeIds)
            .map: planSchemaStates =>
              planSchemaStates ++ boardStatesAndNoticeIds.map(_._1)

      case OrderNoticesRead =>
        order.ifState[ExpectingNotices] match
          case None => Right(Nil)
          case Some(previousOrder) => removeNoticeExpectation(previousOrder)

      case OrderNoticesConsumptionStarted(consumedNoticeIds) =>
        val isConsumption = consumedNoticeIds.toSet
        val noticeIds =
          order.ifState[ExpectingNotices].fold_(Vector.empty, _.state.noticeIds)
            .concat(consumedNoticeIds).distinct
        strictly(noticeIds.areUniqueBy(_.boardPath))
        noticeIds.traverse: noticeId =>
          for
            boardState <- keyTo(BoardState).checked(noticeId.boardPath)
            boardState <-
              if isConsumption(noticeId) then
                boardState.startConsumption(noticeId.plannedNoticeKey, orderId)
              else
                boardState.removeExpectation(noticeId.plannedNoticeKey, orderId)
          yield
            boardState -> noticeId.plannedNoticeKey
        .flatMap: boardStatesAndPlannedNoticeIds =>
          updateNoticeIdsInPlans(boardStatesAndPlannedNoticeIds)
            .map: planSchemaStates =>
              planSchemaStates ++ boardStatesAndPlannedNoticeIds.map(_._1)

      case OrderNoticesConsumed(failed) =>
        for
          consumeNotices <-
            if failed then
              findInstructionInCallStack[ConsumeNotices](order.workflowPosition)
            else
              // When succeeding, the Order must be in the instruction block just below
              // the ConsumeNotice instruction.
              order.workflowPosition.checkedParent.flatMap(instruction_[ConsumeNotices])
          itemStates <-
            consumeNotices.referencedBoardPaths.toVector.traverse: boardPath =>
              for
                boardState <- keyTo(BoardState).checked(boardPath)
                (boardState, maybeNoticeId) <-
                  boardState.finishConsumption(orderId, succeeded = !failed)
              yield
                boardState -> maybeNoticeId
            .flatMap: boardStatesAndMaybeNoticeIds =>
              val boardStatesAndNoticeIds = boardStatesAndMaybeNoticeIds.collect:
                case (boardState, Some(plannedNoticeKey)) => boardState -> plannedNoticeKey
              updateNoticeIdsInPlans(boardStatesAndNoticeIds)
                .map: planSchemaStates =>
                  val boardStates = boardStatesAndMaybeNoticeIds.map(_._1)
                  planSchemaStates ++ boardStates: Seq[BoardState | PlanSchemaState]
        yield
          itemStates

  private def findInstructionInCallStack[I <: Instruction: ClassTag](
    workflowPosition: WorkflowPosition)
  : Checked[I] =
    for
      pos <- workflowPosition.checkedParent
      instr <- instruction_[I](pos).orElse(findInstructionInCallStack[I](pos))
    yield
      instr

  protected final def checkChangedItem(item: UnsignedSimpleItem): Checked[Unit] =
    Checked.unit

  private def removeNoticeExpectation(order: Order[ExpectingNotices])
  : Checked[Seq[BoardState | PlanSchemaState]] =
    order.state.noticeIds.traverse: noticeId =>
      for
        boardState <- keyTo(BoardState).checked(noticeId.boardPath)
        boardState <- boardState.removeExpectation(noticeId.plannedNoticeKey, order.id)
      yield
        boardState -> noticeId.plannedNoticeKey
    .flatMap: boardStatesAndNoticeKeys =>
      updateNoticeIdsInPlans(boardStatesAndNoticeKeys)
        .map: planSchemaStates =>
          val boardStates = boardStatesAndNoticeKeys.map(_._1)
          planSchemaStates ++ boardStates

  protected final def updateNoticeIdInPlan(noticeId: NoticeId, boardState: BoardState)
  : Checked[Option[PlanSchemaState]] =
    updateNoticeIdsInPlans(boardState -> noticeId.plannedNoticeKey :: Nil)
      .map: planSchemaStates =>
        assertThat(planSchemaStates.sizeIs <= 1)
        planSchemaStates.headOption

  protected def updateNoticeIdsInPlans(
    boardStateAndNoticeIds: Seq[(BoardState, PlannedNoticeKey)])
  : Checked[Seq[PlanSchemaState]]


object EventDrivenStateView:
  trait Companion[Self <: EventDrivenStateView[Self]]
  extends EventDrivenState.Companion[Self]
