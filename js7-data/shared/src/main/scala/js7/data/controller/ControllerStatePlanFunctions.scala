package js7.data.controller

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.StandardMapView
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardPath, BoardState, PlannedNoticeKey}
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
        case (Right(templatePlanState), (boardState, noticeId)) =>
          if boardState.containsNoticeKey(noticeId) then
            templatePlanState.addNoticeKey(planKey, boardState.path, noticeId.noticeKey)
          else
            Right:
              templatePlanState.removeNoticeKey(planKey, boardState.path, noticeId.noticeKey)
      .last

  final def deleteBoardInPlanSchemaStates(boardPath: BoardPath): View[PlanSchemaState] =
    keyTo(PlanSchemaState).values.view.map:
      _.deleteBoard(boardPath)

  final def deleteNoticesOfDeadPlan(planId: PlanId): View[KeyedEvent[NoticeDeleted]] =
    if planId.isGlobal then
      View.empty
    else
      toPlan.get(planId).view.flatMap(_.deleteDeadNoticeIds)

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
  final def templateToKeyToPlan: MapView[PlanSchemaId, Map[PlanKey, Plan]] =
    val toPlanSchemaState = keyTo(PlanSchemaState)
    new StandardMapView[PlanSchemaId, Map[PlanKey, Plan]]:
      override def keySet = toPlanSchemaState.keySet

      def get(planSchemaId: PlanSchemaId) =
        toPlanSchemaState.get(planSchemaId).map(_.toPlan)
