package js7.data.controller

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.StandardMapView
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardPath, BoardState, Notice, NoticeId, NoticePlace, PlannedNoticeKey}
import js7.data.event.KeyedEvent
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

  protected final def updateNoticePlacesInPlan(
    planId: PlanId,
    boardStateAndNoticeIds: Seq[(BoardState, PlannedNoticeKey)])
  : Checked[PlanSchemaState] =
    val PlanId(planSchemaId, planKey) = planId
    keyTo(PlanSchemaState).checked(planSchemaId).flatMap: templatePlanState =>
      boardStateAndNoticeIds.scanLeft(Checked(templatePlanState)):
        case (left @ Left(_), _) => left
        case (Right(templatePlanState), (boardState, plannedNoticeKey)) =>
          if boardState.containsNoticeKey(plannedNoticeKey) then
            templatePlanState.addNoticeKey(planKey, boardState.path, plannedNoticeKey.noticeKey)
          else
            Right:
              templatePlanState.removeNoticeKey(planKey, boardState.path, plannedNoticeKey.noticeKey)
      .last

  final def removeBoardInPlanSchemaStates(boardPath: BoardPath): View[PlanSchemaState] =
    keyTo(PlanSchemaState).values.view.map:
      _.removeBoard(boardPath)

  final def removeNoticesOfDeadPlan(planId: PlanId): View[KeyedEvent[NoticeDeleted]] =
    if planId.isGlobal then
      View.empty
    else
      toPlan.get(planId).view.flatMap(_.removeDeadNoticeIds)

  /** Returns Right(()) iff the denoted PlanSchema is unused. */
  final def checkPlanSchemaIsDeletable(planSchemaId: PlanSchemaId): Checked[Unit] =
    keyTo(PlanSchemaState).checked(planSchemaId).flatMap(_.checkIsDeletable)

  @TestOnly
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
        keyTo(BoardState).values.view.flatMap: boardState =>
          boardState.toNoticePlace.keys.map(boardState.path / _)
        .toSet

      def get(noticeId: NoticeId): Option[NoticePlace] =
        maybeNoticePlace(noticeId)

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
