package js7.data.controller

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichPartialFunction}
import js7.base.utils.StandardMapView
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticeMoved}
import js7.data.board.{BoardItem, BoardPath, BoardState, GlobalBoard, Notice, NoticeId, NoticePlace, PlannableBoard, PlannedNoticeKey}
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, KeyedEvent}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.order.OrderEvent.OrderStateReset
import js7.data.plan.{Plan, PlanId, PlanKey, PlanSchemaId, PlanSchemaState}
import js7.data.state.EventDrivenStateView
import org.jetbrains.annotations.TestOnly
import scala.collection.{MapView, View}

/** Contains some of the ControllerState's Plan functions.
  */
transparent trait ControllerStatePlanFunctions
// Extend EventDrivenStateView to make abstract function implementation visible:
extends EventDrivenStateView[ControllerState]:
  this: ControllerState =>

  protected final def updateNoticeIdsInPlans(
    boardStateAndNoticeIds: Seq[(BoardState, PlannedNoticeKey)])
  : Checked[Seq[PlanSchemaState]] =
    boardStateAndNoticeIds.groupBy(_._2.planId).toVector
      .traverse: (planId, boardStateAndNoticeIds) =>
        import planId.{planKey, planSchemaId}
        keyTo(PlanSchemaState).checked(planSchemaId).flatMap: planSchemaState =>
          boardStateAndNoticeIds.scanLeft(Checked(planSchemaState)):
            case (left @ Left(_), _) => left
            case (Right(planSchemaState), (boardState, plannedNoticeKey)) =>
              if boardState.containsNoticeKey(plannedNoticeKey) then
                planSchemaState.addNoticeKey(planKey, boardState.path / plannedNoticeKey.noticeKey)
              else
                Right:
                  planSchemaState.removeNoticeKey(planKey, boardState.path / plannedNoticeKey.noticeKey)
          .last

  final def removeBoardInPlanSchemaStates(boardPath: BoardPath): View[PlanSchemaState] =
    keyTo(PlanSchemaState).values.view.map:
      _.removeBoard(boardPath)

  /** Return NoticeDeleted events if Plan isDead. */
  final def deadPlanNoticeDeleted(planId: PlanId): View[KeyedEvent[NoticeDeleted]] =
    toPlan.get(planId).view.flatMap(_.deadNoticeDeleted)

  /** Returns Right(()) iff the denoted PlanSchema is unused. */
  final def checkPlanSchemaIsDeletable(planSchemaId: PlanSchemaId): Checked[Unit] =
    keyTo(PlanSchemaState).checked(planSchemaId).flatMap(_.checkIsDeletable)

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
  final def schemaToKeyToPlan: MapView[PlanSchemaId, Map[PlanKey, Plan]] =
    val toPlanSchemaState = keyTo(PlanSchemaState)
    new StandardMapView[PlanSchemaId, Map[PlanKey, Plan]]:
      override def keySet = toPlanSchemaState.keySet

      def get(planSchemaId: PlanSchemaId) =
        toPlanSchemaState.get(planSchemaId).map(_.toPlan)

  @TestOnly
  private def toNotice: MapView[NoticeId, Notice] =
    new StandardMapView[NoticeId, Notice]:
      private val toNoticePlace = ControllerStatePlanFunctions.this.toNoticePlace

      override def keySet: Set[NoticeId] =
        keyTo(BoardState).values.view.flatMap: boardState =>
          boardState.toNoticePlace.view.collect:
            case (plannedNoticeKey, NoticePlace(Some(_), _, _, _, _)) =>
              boardState.path / plannedNoticeKey
        .toSet

      def get(noticeId: NoticeId): Option[Notice] =
        toNoticePlace.get(noticeId).flatMap(_.notice)

  @TestOnly
  def toNoticePlace: MapView[NoticeId, NoticePlace] =
    new StandardMapView[NoticeId, NoticePlace]:
      override def keySet: Set[NoticeId] =
        allNoticeIds.toSet

      def get(noticeId: NoticeId): Option[NoticePlace] =
        maybeNoticePlace(noticeId)

  def allNoticeIds: View[NoticeId] =
    keyTo(BoardState).values.view.flatMap: boardState =>
      boardState.toNoticePlace.keys.view.map(boardState.path / _)

  def allNoticePlaces: View[(NoticeId, NoticePlace)] =
    keyTo(BoardState).values.view.flatMap: boardState =>
      boardState.toNoticePlace.view.map: (k, v) =>
        boardState.path / k -> v

  def maybeNoticePlace(noticeId: NoticeId): Option[NoticePlace] =
    for
      boardState <- keyTo(BoardState).get(noticeId.boardPath)
      noticePlace <- boardState.toNoticePlace.get(noticeId.plannedNoticeKey)
    yield
      noticePlace

  //@TestOnly
  //def noticeIdExists(noticeId: NoticeId): Boolean =
  //  keyTo(BoardState).get(noticeId.boardPath).fold(false):boardState =>
  //    boardState.toNoticePlace.contains(noticeId.plannedNoticeKey)


object ControllerStatePlanFunctions:

  def changeBoardType(
    newBoard: BoardItem,
    planSchemaId: PlanSchemaId,
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
          changeBoardTypeOrderAndNoticeEvents(boardState, planSchemaId, endOfLife, convertNoticeKey)
      yield
        coll

  private def checkIsOtherBoardType(board: BoardItem, newBoard: BoardItem): Checked[Unit] =
    (board, newBoard) match
      case (_: PlannableBoard, _: GlobalBoard) => Right(())
      case (_: GlobalBoard, _: PlannableBoard) => Right(())
      case _ => Left(Problem(s"${board.path} is already of the request Board type"))

  private def changeBoardTypeOrderAndNoticeEvents(
    boardState: BoardState,
    planSchemaId: PlanSchemaId,
    endOfLife: Option[Timestamp],
    convertNoticeKey: PlannedNoticeKey => Checked[Option[PlannedNoticeKey]])
  : Checked[Seq[KeyedEvent[Event]]] =
    // Update Notices and Orders waiting for Notices
    boardState.toNoticePlace.toVector.flatTraverse: (fromPlannedNoticeKey, noticePlace) =>
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
              boardState.path <-: NoticeMoved(
                fromPlannedNoticeKey = fromPlannedNoticeKey,
                toPlannedNoticeKey = toPlannedNoticeKey,
                endOfLife = endOfLife)
          orderResets ++ maybeNoticeMoved
