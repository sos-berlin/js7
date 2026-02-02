package js7.data.board

import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.{RichIterable, RichIterableOnce}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{L3, ScalaUtils}
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{BoardPath, BoardState, GlobalBoard, NoticeId, PlannableBoard}
import js7.data.controller.{ControllerCommand, ControllerState}
import js7.data.event.{EventCalc, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource.moveOrderToNextInstruction
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticeAnnounced, OrderNoticeEvent, OrderNoticePosted, OrderNoticesConsumptionStarted, OrderNoticesRead}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId}
import js7.data.plan.{PlanId, PlanSchemaState}
import js7.data.state.EngineEventColl.extensions.order
import js7.data.state.EngineState_
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{DontWait, SkipWhenNoNotice, Wait}
import js7.data.workflow.instructions.{ConsumeNotices, ExpectNotices, ExpectOrConsumeNoticesInstruction, PostNotices}
import scala.reflect.ClassTag

final class NoticeEventSource(forCommand: Boolean):

  private val logger = Logger[this.type]

  def postNotices(boardPaths: Vector[BoardPath], order: Order[Order.Ready])
  : EventCalc[ControllerState, OrderNoticeEvent | OrderMoved] =
    EventCalc: coll =>
      for
        _ <- boardPaths.checkUniqueness
        boardStates <- boardPaths.traverse(coll.aggregate.keyTo(BoardState).checked)
        notices <- boardStates.traverse: boardState =>
          boardState.board.match
            case board: GlobalBoard =>
              board.postingOrderToNotice(order, coll.aggregate, coll.now)

            case board: PlannableBoard =>
              board.postingOrderToNotice(order, coll.aggregate)
        coll <- coll:
          toPostingOrderEvents(notices, order)
        coll <- coll:
          toExpectingOrderEvents(order.planId, notices)
      yield
        coll

  def executePostNoticeCommand(postNotice: ControllerCommand.PostNotice)
  : EventCalc[ControllerState, OrderNoticeEvent | OrderMoved | NoticeEvent] =
    import postNotice.endOfLife
    import postNotice.noticeId.{boardPath, plannedNoticeKey}
    EventCalc: coll =>
      for
        boardState <- coll.aggregate.keyTo(BoardState).checked(boardPath)
        notice <- boardState.board.toNotice(plannedNoticeKey, endOfLife)(NowScope(coll.now))
        _ <- coll.aggregate.updatePlannedBoard(postNotice.noticeId.plannedBoardId):
          _.addNotice(notice) // Check only
        coll <- coll:
          NoticePosted.toKeyedEvent(notice)
        coll <-
          if endOfLife.exists(_ <= coll.now) then
            logger.debug(s"Delete $notice immediately because endOfLife has been reached")
            coll:
              boardPath <-: NoticeDeleted(plannedNoticeKey)
          else
            coll:
              postedNoticeToExpectingOrderEvents(notice).widen
      yield
        coll

  private def toPostingOrderEvents[S <: EngineState_[S]](
    notices: Vector[Notice],
    order: Order[Order.State])
  : EventCalc[S, OrderNoticePosted | OrderMoved] =
      notices.map: notice =>
        EventCalc.pure:
          order.id <-: OrderNoticePosted(notice.id, notice.endOfLife)
      .appended:
        moveOrderToNextInstruction(order, forCommand = forCommand).widen
      .foldMonoids

  // For PostNotice command
  private def postedNoticeToExpectingOrderEvents(notice: Notice)
  : EventCalc[ControllerState, OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    toExpectingOrderEvents(notice.planId, Vector(notice))

  private def toExpectingOrderEvents(
    planId: PlanId,
    postedNotices: Vector[Notice])
  : EventCalc[ControllerState, OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    EventCalc: coll =>
      for
        _ <- postedNotices.map(_.boardPath).checkUniqueness
        expectingOrders <- postedNotices
          .traverse: notice =>
            coll.aggregate.toPlannedBoard(notice.boardPath, planId).map:
              _.expectingOrders(notice.noticeKey)
          .flatMap:
            _.flatten.distinct.traverse:
              coll.aggregate.idToOrder.checked
        coll <-
          coll.addCheckedEventCalc:
            expectingOrders.traverse: expectingOrder =>
              coll.aggregate
                .instruction_[ExpectOrConsumeNoticesInstruction](expectingOrder.workflowPosition)
                .map(expectingOrder -> _)
            .flatMap:
              _.traverse: (expectingOrder, expectNoticesInstr) =>
                coll.order(expectingOrder.id)
                  .flatMap(_.checkedState[Order.ExpectingNotices])
                  .map: expectingOrder =>
                    val postedNoticeIds = postedNotices.map(_.id).toSet
                    tryFulfillExpectingOrder(expectNoticesInstr,
                      expectingOrder, coll.aggregate.isNoticeAvailable, postedNoticeIds)
            .map(EventCalc.combineAll)
      yield
        coll

  def tryFulfillExpectingOrder(
    instr: ExpectOrConsumeNoticesInstruction,
    order: Order[Order.ExpectingNotices],
    isNoticeAvailable: NoticeId => L3,
    isPostedNow: Set[NoticeId] = Set.empty)
  : EventCalc[ControllerState, OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    tryFulfill(instr, order, order.state.noticeIds, isNoticeAvailable, isPostedNow)

  def tryFulfill(
    instr: ExpectOrConsumeNoticesInstruction,
    order: Order[Order.Ready | Order.ExpectingNotices],
    noticeIds: Vector[NoticeId],
    isNoticeAvailable: NoticeId => L3,
    isPostedNow: Set[NoticeId] = Set.empty)
  : EventCalc[ControllerState, OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    val boardToL3 =
      noticeIds.map: noticeId =>
        noticeId.boardPath -> locally:
          if isPostedNow(noticeId) then
            L3.True
          else
            isNoticeAvailable(noticeId)
      .toMap
    if instr.boardPaths.eval(boardToL3) != L3.False then
      val consumedNoticeIds = noticeIds.filter:
        o => isPostedNow(o) || isNoticeAvailable(o) == L3.True
      fulfilledEvents(instr, order, noticeIds, consumedNoticeIds, instr.boardPaths.eval(boardToL3))
    else
      EventCalc.empty

  private def fulfilledEvents[S <: EngineState_[S]](
    instr: ExpectOrConsumeNoticesInstruction,
    order: Order[Order.Ready | Order.ExpectingNotices],
    noticeIds: Vector[NoticeId],
    consumedNoticeIds: Vector[NoticeId],
    exprResult: L3)
  : EventCalc[S, OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    instr match
      case instr: ConsumeNotices =>
        fulfilledEventsConsumeNotices(instr, order, noticeIds, consumedNoticeIds, exprResult)
      case instr: ExpectNotices =>
        fulfilledEventsExpectNotices(instr, order, noticeIds, consumedNoticeIds, exprResult).widen

  private def fulfilledEventsConsumeNotices[S <: EngineState_[S]](
    instr: ConsumeNotices,
    order: Order[Order.Ready | Order.ExpectingNotices],
    noticeIds: Vector[NoticeId],
    consumedNoticeIds: Vector[NoticeId],
    exprResult: L3)
  : EventCalc[S, OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    exprResult match
      case L3.False =>
        EventCalc.empty

      case L3.True =>
        EventCalc.pure:
          order.id <-: OrderNoticesConsumptionStarted(noticeIds)

      case L3.Unknown =>
        instr.whenNotAnnounced match
          case Wait => EventCalc.empty

          case SkipWhenNoNotice if consumedNoticeIds.isEmpty =>
            EventCalc: coll =>
              for
                coll <- coll:
                  order.id <-: OrderNoticesRead
                coll <- coll:
                  moveOrderToNextInstruction(order, Some(OrderMoved.NoNotice),
                    forCommand = forCommand)
              yield coll

          case _ =>
            EventCalc.pure:
              order.id <-: OrderNoticesConsumptionStarted(noticeIds)

  private def fulfilledEventsExpectNotices[S <: EngineState_[S]](
    instr: ExpectNotices,
    order: Order[Order.Ready | Order.ExpectingNotices],
    noticeIds: Vector[NoticeId],
    consumedNoticeIds: Vector[NoticeId],
    exprResult: L3)
  : EventCalc[S, OrderNoticesRead | OrderMoved] =
    exprResult.match
      case L3.False =>
        // Notices don't match but are announced
        EventCalc.empty

      case L3.True =>
        // Notices match
        EventCalc: coll =>
          for
            coll <- coll:
              order.id <-: OrderNoticesRead
            coll <- coll:
              moveOrderToNextInstruction(order, forCommand = forCommand)
          yield coll

      case L3.Unknown =>
        // Notices neither match nor are they announced
        instr.whenNotAnnounced match
          case Wait =>
            EventCalc.empty

          case DontWait | SkipWhenNoNotice =>
            EventCalc: coll =>
              for
                coll <- coll:
                  order.id <-: OrderNoticesRead
                coll <- coll:
                  moveOrderToNextInstruction(order, Some(OrderMoved.NoNotice),
                    forCommand = forCommand)
              yield coll


object NoticeEventSource:

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
