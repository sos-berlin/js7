package js7.data.state

import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.Problems.EventNotHandledHereProblem
import js7.data.event.{Event, EventDrivenState, EventDrivenState_}
import js7.data.item.{UnsignedSimpleItem, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.order.Order.{ExpectingNotices, WaitingForLock}
import js7.data.order.OrderEvent.{OrderAddedX, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderDetached, OrderExternalVanished, OrderForked, OrderJoined, OrderLockEvent, OrderLocksAcquired, OrderLocksQueued, OrderLocksReleased, OrderOrderAdded, OrderStateReset, OrderStdWritten}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.plan.PlanSchemaState
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.LockInstruction
import js7.data.workflow.position.WorkflowPosition
import scala.reflect.ClassTag

/** The common part of ControllerState and AgentState. */
trait EventDrivenStateView
extends EventDrivenState[Event], EngineState:
  type This <: EventDrivenStateView_[This]

  protected def addOrders(orders: Seq[Order[Order.State]] = Nil, allowClosedPlan: Boolean)
  : Checked[This]

  protected final def update(
    updateOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[This] =
    for
      _ <- precheckUpdate(updateOrders, removeOrders, externalVanishedOrders,
        addItemStates, removeUnsignedSimpleItems)
      self <- update_(updateOrders, removeOrders, externalVanishedOrders,
        addItemStates, removeUnsignedSimpleItems)
    yield
      self

  // Call only via update() !
  protected def update_(
    updateOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[This]

  private def precheckUpdate(
    updatedOrders: Seq[Order[Order.State]] = Nil,
    removeOrders: Seq[OrderId] = Nil,
    externalVanishedOrders: Seq[Order[Order.State]] = Nil,
    addItemStates: Seq[UnsignedSimpleItemState] = Nil,
    removeUnsignedSimpleItems: Seq[UnsignedSimpleItemPath] = Nil)
  : Checked[Unit] =
    if isStrict then
      for
        _ <- updatedOrders.view.map(_.id).concat(removeOrders).checkUniqueness
        _ <- externalVanishedOrders.checkUniquenessBy(_.id)
        _ <- addItemStates.view.map(_.path).concat(removeUnsignedSimpleItems).checkUniqueness
      yield ()
    else
      Checked.unit

  protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[This]

  protected def applyOrderCoreEvent(orderId: OrderId, event: OrderCoreEvent): Checked[This]

  protected final def findInstructionInCallStack[I <: Instruction: ClassTag](
    workflowPosition: WorkflowPosition)
  : Checked[I] =
    for
      pos <- workflowPosition.checkedParent
      instr <- instruction_[I](pos).orElse(findInstructionInCallStack[I](pos))
    yield
      instr

  protected final def checkChangedItem(item: UnsignedSimpleItem): Checked[Unit] =
    Checked.unit

  protected def removeNoticeExpectation(order: Order[ExpectingNotices])
  : Checked[Seq[PlanSchemaState]] =
    Left(Problem(s"removeNoticeExpectation not implemented for $companion"))


object EventDrivenStateView:
  trait Companion[T <: EventDrivenStateView_[T]]
  extends EventDrivenState.Companion[T]:
    override def updateStaticReference(engineState: T): Unit =
      EngineStateMXBean.setEngineState(engineState)


trait EventDrivenStateView_[T <: EventDrivenStateView_[T]]
extends EventDrivenStateView, EventDrivenState_[T, Event]:
  this: T =>
  type This = T

  protected def applyOrderEvent(orderId: OrderId, event: OrderEvent): Checked[This] =
    event match
      case _: OrderAddedX | _: OrderEvent.OrderAttachedToAgent =>
        // Event is handled by one of ControllerState and AgentState only
        Left(EventNotHandledHereProblem(event, companion))

      case event: OrderCoreEvent =>
        applyOrderCoreEvent(orderId, event)

      case _: OrderStdWritten =>
        // OrderStdWritten is not applied. But check OrderId.
        idToOrder.checked(orderId).rightAs(this)

  protected final def applyOrderCoreEvent(orderId: OrderId, event: OrderCoreEvent): Checked[This] =
    for
      previousOrder <- idToOrder.checked(orderId)
      updatedOrder <- previousOrder.applyEvent(event)
      result <- event match
        case OrderDetached =>
          if isAgent then
            update(removeOrders = orderId :: Nil)
          else
            update(updateOrders = updatedOrder :: Nil)

        case event: OrderForked =>
          update(updatedOrder :: Nil).flatMap:
            _.addOrders(previousOrder.newForkedOrders(event), allowClosedPlan = true)

        case event: OrderJoined =>
          if isAgent then
            eventNotApplicable(orderId <-: event)
          else
            previousOrder.state match
              case forked: Order.Forked =>
                update(
                  updateOrders = updatedOrder :: Nil,
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
                updateOrders = updatedOrder :: Nil,
                addItemStates = lockStates)

        case OrderStateReset =>
          previousOrder.ifState[WaitingForLock].map: order =>
            val instr = instruction_[LockInstruction](order.workflowPosition).orThrow
            foreachLock(instr.lockPaths): lockState =>
              Right:
                lockState.dequeue(orderId)
            .flatMap: lockStates =>
              update(
                updateOrders = updatedOrder :: Nil,
                addItemStates = lockStates)
          .orElse:
            previousOrder.ifState[ExpectingNotices].map: order =>
              removeNoticeExpectation(order).flatMap: updatedBoardStates =>
                update(
                  updateOrders = updatedOrder :: Nil,
                  addItemStates = updatedBoardStates)
          .getOrElse:
            update(
              updateOrders = updatedOrder :: Nil)

        case _: OrderCancelled =>
          previousOrder
            // COMPATIBLE Since v2.7.2 an OrderStateReset is emitted and the
            // following code is superfluous (but still needed for old journals)
            .ifState[ExpectingNotices]
            .fold(update(updateOrders = updatedOrder :: Nil)): order =>
              removeNoticeExpectation(order).flatMap: updatedBoardStates =>
                update(
                  updateOrders = updatedOrder :: Nil,
                  addItemStates = updatedBoardStates)

        case OrderExternalVanished =>
          if updatedOrder.externalOrder.isEmpty then
            Left(Problem(s"OrderExternalVanished but $orderId is not linked to an external order"))
          else
            update(externalVanishedOrders = updatedOrder :: Nil)

        case OrderDeletionMarked =>
          update(updateOrders = updatedOrder :: Nil)

        case OrderDeleted =>
          if isAgent then
            eventNotApplicable(orderId <-: event)
          else
            update(removeOrders = orderId :: Nil)

        case event: OrderOrderAdded =>
          // ControllerState handles this event
          Left(EventNotHandledHereProblem(event, companion))

        case _ =>
          update(updateOrders = updatedOrder :: Nil)
    yield
      result
