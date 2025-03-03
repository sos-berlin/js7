package js7.data.board

import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.NoticeEventSource.*
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticeAnnounced, OrderNoticeEvent, OrderNoticePosted, OrderNoticesConsumptionStarted, OrderNoticesRead}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchemaState}
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.{ExpectOrConsumeNoticesInstruction, PostNotices}

final class NoticeEventSource(clock: WallClock):

  def postNotices(
    boardPaths: Vector[BoardPath],
    order: Order[Order.Ready],
    controllerState: ControllerState)
  : Checked[List[KeyedEvent[OrderNoticeEvent | OrderMoved]]] =
    for
      _ <- boardPaths.checkUniqueness
      boardStates <- boardPaths.traverse(controllerState.keyTo(BoardState).checked)
      notices <- boardStates.traverse: boardState =>
        boardState.board.match
          case board: GlobalBoard =>
            board.postingOrderToNotice(order, controllerState, clock.now())

          case board: PlannableBoard =>
            board.postingOrderToNotice(order, controllerState)
      postingOrderEvents = toPostingOrderEvents(notices, order)
      expectingOrderEvents <- toExpectingOrderEvents(order.planId, notices, controllerState)
    yield
      postingOrderEvents ++: expectingOrderEvents.toList

  def executePostNoticeCommand(
    postNotice: ControllerCommand.PostNotice,
    controllerState: ControllerState)
  : Checked[Seq[KeyedEvent[OrderNoticeEvent | OrderMoved | NoticeEvent]]] =
    import postNotice.endOfLife
    import postNotice.noticeId.{boardPath, plannedNoticeKey}
    val scope = NowScope(clock.now()) // TODO Should be pure ?
    for
      boardState <- controllerState.keyTo(BoardState).checked(boardPath)
      notice <- boardState.board.toNotice(plannedNoticeKey, endOfLife)(scope)
      _ <- controllerState.updatePlannedBoard(postNotice.noticeId.plannedBoardId):
        _.addNotice(notice) // Check only
      expectingOrderEvents <-
        if endOfLife.exists(_ <= clock.now()) then
          logger.debug(s"Delete $notice immediately because endOfLife has been reached")
          Right((boardPath <-: NoticeDeleted(plannedNoticeKey)) :: Nil)
        else
          postedNoticeToExpectingOrderEvents(notice, controllerState)
    yield
      NoticePosted.toKeyedEvent(notice) +: expectingOrderEvents


object NoticeEventSource:

  private val logger = Logger[this.type]


  /** Returns the OrderNoticeAnnounced events required for an added posting order. */
  def planToNoticeAnnounced(
    planId: PlanId,
    order: FreshOrder,
    innerBlock: Workflow,
    controllerState: ControllerState)
  : Checked[List[OrderNoticeAnnounced]] =
    if planId.isGlobal then
      Right(Nil)
    else
      innerBlock.instructions.view.collect:
        case postNotices: PostNotices =>
          controllerState.keyTo(PlanSchemaState).checked(planId.planSchemaId)
            .flatMap: planSchemaState =>
              planSchemaState.plan(planId.planKey).flatMap: plan =>
                postNotices.boardPaths.traverse: boardPath =>
                  controllerState.keyTo(BoardState).checked(boardPath).flatMap: boardState =>
                    boardState.item match
                      case _: GlobalBoard => Right(None)
                      case plannableBoard: PlannableBoard =>
                        plannableBoard.freshOrderToNoticeKey(order, controllerState)
                          .map: noticeKey =>
                            !plan.isNoticeAnnounced(boardPath / noticeKey) thenSome:
                              OrderNoticeAnnounced(planId / boardPath / noticeKey)
          .map(_.flatten)
      .toList.sequence
      .map(_.flatten.distinct)

  private def toPostingOrderEvents(notices: Vector[Notice], order: Order[Order.State])
  : Vector[KeyedEvent[OrderNoticePosted | OrderMoved]] =
    notices.map: notice =>
      order.id <-: OrderNoticePosted(notice.id, notice.endOfLife)
    .appended:
      order.id <-: OrderMoved(order.position.increment)

  // For PostNotice command
  private def postedNoticeToExpectingOrderEvents(
    notice: Notice,
    controllerState: ControllerState)
  : Checked[Seq[KeyedEvent[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]]] =
    toExpectingOrderEvents(notice.planId, Vector(notice), controllerState)

  private def toExpectingOrderEvents(
    planId: PlanId,
    postedNotices: Vector[Notice],
    controllerState: ControllerState)
  : Checked[Vector[KeyedEvent[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]]] =
    for
      _ <- postedNotices.map(_.boardPath).checkUniqueness
      planSchemaState <- controllerState.keyTo(PlanSchemaState).checked(planId.planSchemaId)
      plan <- planSchemaState.plan(planId.planKey)
      expectingOrders <- postedNotices
        .flatMap: notice =>
          plan.plannedBoard(notice.boardPath).expectingOrders(notice.noticeKey)
        .distinct
        .traverse(controllerState.idToOrder.checked)
      events <- expectingOrders
        .traverse: expectingOrder =>
          controllerState.instruction_[ExpectOrConsumeNoticesInstruction](expectingOrder.workflowPosition)
            .map(expectingOrder -> _)
        .flatMap:
          _.traverse: (expectingOrder, expectNoticesInstr) =>
            controllerState.idToOrder.checked(expectingOrder.id)
              .flatMap(_.checkedState[Order.ExpectingNotices])
              .map: expectingOrder =>
                val postedNoticeIds = postedNotices.map(_.id).toSet
                expectNoticesInstr
                  .tryFulfillExpectingOrder(expectingOrder, controllerState.isNoticeAvailable, postedNoticeIds)
                  .map(expectingOrder.id <-: _)
          .map(_.flatten)
    yield
      events
