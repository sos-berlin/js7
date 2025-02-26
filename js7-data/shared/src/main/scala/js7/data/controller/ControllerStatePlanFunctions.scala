package js7.data.controller

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{L3, StandardMapView}
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticeMoved}
import js7.data.board.{BoardItem, BoardPath, BoardState, GlobalBoard, Notice, NoticeEvent, NoticeId, NoticeKey, NoticePlace, PlannableBoard, PlannedBoard, PlannedBoardId, PlannedNoticeKey}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, KeyedEvent}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent.OrderStateReset
import js7.data.order.{Order, OrderEvent}
import js7.data.plan.PlanEvent.{PlanDeleted, PlanFinished}
import js7.data.plan.{Plan, PlanId, PlanKey, PlanSchemaId, PlanSchemaState}
import js7.data.state.EventDrivenStateView
import org.jetbrains.annotations.TestOnly
import scala.collection.{MapView, View}

/** Contains some of the ControllerState's Plan functions.
  */
transparent trait ControllerStatePlanFunctions[Self <: ControllerStatePlanFunctions[Self]]
// Extend EventDrivenStateView to make abstract function implementation visible:
extends EventDrivenStateView[Self]:
  this: Self =>

  override protected def removeNoticeExpectation(order: Order[ExpectingNotices])
  : Checked[Seq[PlanSchemaState]] =
    order.state.noticeIds.groupBy(_.planSchemaId).toVector.traverse: (planSchemaId, noticeIds) =>
      for
        planSchemaState <- keyTo(PlanSchemaState).checked(planSchemaId)
        planSchemaState <-
          noticeIds.scanLeft(Checked(planSchemaState)): (checkedPlanSchemaState, noticeId) =>
            checkedPlanSchemaState.flatMap:
              _.updatePlannedBoard(noticeId.planKey, noticeId.boardPath):
                _.removeExpectation(noticeId.noticeKey, order.id)
          .last
      yield
        planSchemaState

  protected final def updatePlannedBoards(noticeIds: Seq[NoticeId])
    (updateItemStates: (PlannedBoard, NoticeKey) => Checked[PlannedBoard])
  : Checked[Seq[PlanSchemaState]] =
    noticeIds.groupBy(_.planSchemaId)
      .view.mapValues: noticeIds =>
        noticeIds.groupBy(_.planKey).view.mapValues: noticeIds =>
          noticeIds.groupMap(_.boardPath)(_.noticeKey)
      .toVector.traverse: (planSchemaId, planKeyToBoardToNoticeIds) =>
        keyTo(PlanSchemaState).checked(planSchemaId).flatMap: planSchemaState =>
          planKeyToBoardToNoticeIds.toVector.traverse: (planKey, boardToNoticeKeys) =>
            planSchemaState.plan(planKey).flatMap: plan =>
              boardToNoticeKeys.toVector.traverse: (boardPath, noticeKeys) =>
                val plannedBoard = plan.plannedBoard(boardPath)
                noticeKeys match
                  case Seq(noticeKey) => updateItemStates(plannedBoard, noticeKey)
                  case _ => Left(Problem(s"Exactly one NoticeKey per PlannedBoard expected: {${
                    noticeKeys.map(plannedBoard.id / _ )}"))
              .map: plannedBoards =>
                plan.copy(toPlannedBoard = plan.toPlannedBoard ++ plannedBoards.map(o => o.boardPath -> o))
          .map: plans =>
            planSchemaState.copy(toPlan = planSchemaState.toPlan ++ plans.map(o => o.id.planKey -> o))

  final def updatePlannedBoard(plannedBoardId: PlannedBoardId)
    (updateItemStates: PlannedBoard => Checked[PlannedBoard])
  : Checked[PlanSchemaState] =
    import plannedBoardId.{boardPath, planKey, planSchemaId}
    for
      planSchemaState <- keyTo(PlanSchemaState).checked(planSchemaId)
      planSchemaState <- planSchemaState.updatePlannedBoard(planKey, boardPath)(updateItemStates)
    yield
      planSchemaState

  final def toPlannedBoard(plannedBoardId: PlannedBoardId): Checked[PlannedBoard] =
    import plannedBoardId.{boardPath, planKey, planSchemaId}
    for
      planSchemaState <- keyTo(PlanSchemaState).checked(planSchemaId)
      plan <- planSchemaState.plan(planKey)
    yield
      plan.plannedBoard(boardPath)

  final def removeBoardInPlanSchemaStates(boardPath: BoardPath): View[PlanSchemaState] =
    keyTo(PlanSchemaState).values.view.map:
      _.removeBoard(boardPath)

  /** Return PlanFinished event if Plan should be finished. */
  final def maybePlanFinished(planId: PlanId): View[KeyedEvent[PlanFinished | NoticeDeleted | PlanDeleted]] =
    toPlan.get(planId).fold(View.empty): plan =>
      plan.maybePlanFinished

  /** @return L3.True: Notice exists<br>
    *         L3.False: Notice doesn't exist but is announced<br>
    *         L3.Unknown: Notice doesn't exist nor is it announced
    */
  final def isNoticeAvailable(noticeId: NoticeId): L3 =
    maybePlannedBoard(noticeId.plannedBoardId).fold(L3.Unknown): plannedBoard =>
      plannedBoard.isNoticeAvailable(noticeId.noticeKey)

  final def maybePlannedBoard(plannedBoardId: PlannedBoardId): Option[PlannedBoard] =
    toPlan.get(plannedBoardId.planId).flatMap:
      _.toPlannedBoard.get(plannedBoardId.boardPath)

  def plan(planId: PlanId): Checked[Plan] =
    keyTo(PlanSchemaState).checked(planId.planSchemaId).flatMap:
      _.plan(planId.planKey)

  final def toPlan: MapView[PlanId, Plan] =
    val toPlanSchemaState = keyTo(PlanSchemaState)
    new StandardMapView[PlanId, Plan]:
      override def keySet =
        toPlanSchemaState.values.view.flatMap: planSchemaState =>
          planSchemaState.planIds.view.map(planSchemaState.id / _)
        .toSet

      final def get(planId: PlanId) =
        toPlanSchemaState.get(planId.planSchemaId).flatMap(_.toPlan.get(planId.planKey))

  @TestOnly
  def toNoticePlace: MapView[NoticeId, NoticePlace] =
    new StandardMapView[NoticeId, NoticePlace]:
      override def keySet: Set[NoticeId] =
        allNoticePlaceIds.toSet

      def get(noticeId: NoticeId): Option[NoticePlace] =
        maybeNoticePlace(noticeId)

  private def allNoticePlaceIds: View[NoticeId] =
    for
      plan <- toPlan.values.view
      plannedBoard <- plan.toPlannedBoard.values.view
      noticeKey <- plannedBoard.toNoticePlace.keys
    yield
      plannedBoard.id / noticeKey

  private def allNoticePlaces: View[(NoticeId, NoticePlace)] =
    for
      plan <- toPlan.values.view
      plannedBoard <- plan.toPlannedBoard.values.view
      (noticeKey, noticePlace) <- plannedBoard.toNoticePlace.view
    yield
      plannedBoard.id / noticeKey -> noticePlace

  def allNotices: View[Notice] =
    for
      plan <- toPlan.values.view
      plannedBoard <- plan.toPlannedBoard.values.view
      noticePlace <- plannedBoard.toNoticePlace.values.view
      notice <- noticePlace.notice
    yield
      notice

  def maybeNoticePlace(noticeId: NoticeId): Option[NoticePlace] =
    for
      plan <- toPlan.get(noticeId.planId)
      plannedBoard <- plan.toPlannedBoard.get(noticeId.boardPath)
      noticePlace <- plannedBoard.toNoticePlace.get(noticeId.noticeKey)
    yield
      noticePlace


object ControllerStatePlanFunctions:

  export boardTypeChanger.changeBoardType

  private object boardTypeChanger:
    def changeBoardType(
      newBoard: BoardItem,
      fromPlanSchemaId: PlanSchemaId,
      toPlanSchemaId: PlanSchemaId,
      endOfLife: Option[Timestamp],
      controllerState: ControllerState)
      (convertNoticeKey: PlannedNoticeKey => Checked[Option[PlannedNoticeKey]])
    : Checked[Seq[KeyedEvent[Event]]] =
      val boardPath = newBoard.path
      ControllerEventColl.keyedEvents(controllerState): coll =>
        for
          boardState <- controllerState.keyTo(BoardState).checked(boardPath)
          _ <- checkIsOtherBoardType(boardState.item, newBoard)
          coll <- coll.add:
            NoKey <-: UnsignedSimpleItemChanged:
              newBoard.withRevision(Some(boardState.item.nextRevision))
          coll <- coll.addChecked:
            changeBoardTypeOrderAndNoticeEvents(
              newBoard.path,
              fromPlanSchemaId, toPlanSchemaId,
              endOfLife, controllerState,
              convertNoticeKey)
        yield
          coll

    private def checkIsOtherBoardType(board: BoardItem, newBoard: BoardItem): Checked[Unit] =
      (board, newBoard) match
        case (_: PlannableBoard, _: GlobalBoard) => Right(())
        case (_: GlobalBoard, _: PlannableBoard) => Right(())
        case _ => Left(Problem(s"${board.path} is already of the request Board type"))

    private def changeBoardTypeOrderAndNoticeEvents(
      boardPath: BoardPath,
      fromPlanSchemaId: PlanSchemaId,
      toPlanSchemaId: PlanSchemaId,
      endOfLife: Option[Timestamp],
      controllerState: ControllerState,
      convertNoticeKey: PlannedNoticeKey => Checked[Option[PlannedNoticeKey]])
    : Checked[Seq[KeyedEvent[Event]]] =
      // Update Notices and Orders waiting for Notices
      controllerState.keyTo(PlanSchemaState).checked(fromPlanSchemaId).flatMap: fromPlanSchemaState =>
        fromPlanSchemaState.toPlan.values.toVector.flatMap: plan =>
          plan.toPlannedBoard.get(boardPath).map(plan.id -> _)
        .flatTraverse: (planId, plannedBoard) =>
          plannedBoard.toNoticePlace.toVector.flatTraverse: (noticeKey, noticePlace) =>
            val fromPlannedNoticeKey = planId / noticeKey
            convertNoticeKey(fromPlannedNoticeKey).map:
              case None => Vector.empty
              case Some(toPlannedNoticeKey) =>
                val orderResets = noticePlace.expectingOrderIds.toVector.map: orderId =>
                  orderId <-: OrderStateReset
                val maybeNoticeMoved =
                  // OrderStateReset removes OrderId from NoticePlace#expectingOrderIds,
                  // NoticePlace may then be empty and non-existent,
                  // then NoticeMoved (which should not move expectingOrderIds, too)
                  noticePlace.copy(expectingOrderIds = Set.empty).nonEmpty.thenSome:
                    boardPath <-: NoticeMoved(
                      fromPlannedNoticeKey = fromPlannedNoticeKey,
                      toPlannedNoticeKey = toPlannedNoticeKey,
                      endOfLife = endOfLife)
                orderResets ++ maybeNoticeMoved
