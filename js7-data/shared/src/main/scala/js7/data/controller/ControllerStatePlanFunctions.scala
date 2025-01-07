package js7.data.controller

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.StandardMapView
import js7.data.board.{BoardPath, BoardState, NoticeId}
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.event.KeyedEvent
import js7.data.plan.{Plan, PlanId, PlanKey, PlanTemplateId, PlanTemplateState}
import js7.data.state.EventDrivenStateView
import org.jetbrains.annotations.TestOnly
import scala.collection.{MapView, View}

/** Contains some of the ControllerState's Plan functions.
  */
transparent trait ControllerStatePlanFunctions
// Repeat EventDrivenStateView only to make abstract function implementation visibile:
extends EventDrivenStateView[ControllerState]:
  this: ControllerState =>

  protected final def updateNoticePlacesInPlan(
    planId: PlanId,
    boardStateAndNoticeIds: Seq[(BoardState, NoticeId)])
  : Checked[PlanTemplateState] =
    val PlanId(planTemplateId, planKey) = planId
    keyTo(PlanTemplateState).checked(planTemplateId).flatMap: templatePlanState =>
      boardStateAndNoticeIds.view.scanLeft(Checked(templatePlanState)):
        case (left @ Left(_), _) => left
        case (Right(templatePlanState), (boardState, noticeId)) =>
          boardState.idToNotice.get(noticeId) match
            case None =>
              Right:
                templatePlanState.deleteNoticePlace(planKey, boardState.path, noticeId.noticeKey)
            case Some(noticePlace) =>
              templatePlanState.updateNoticePlace(planKey, boardState.path, noticePlace)
      .last

  final def deleteBoardInPlanTemplateStates(boardPath: BoardPath): View[PlanTemplateState] =
    keyTo(PlanTemplateState).values.view.map:
      _.deleteBoard(boardPath)

  final def deleteNoticesOfDeadPlan(planId: PlanId): View[KeyedEvent[NoticeDeleted]] =
    if planId.isGlobal then
      View.empty
    else
      toPlan.get(planId).view.flatMap(_.deleteDeadNoticeIds)

  /** Returns Right(()) iff the denoted PlanTemplate is unused. */
  final def checkPlanTemplateIsDeletable(planTemplateId: PlanTemplateId): Checked[Unit] =
    keyTo(PlanTemplateState).checked(planTemplateId).flatMap(_.checkIsDeletable)

  @TestOnly
  final def toPlan: MapView[PlanId, Plan] =
    val toPlanTemplateState = keyTo(PlanTemplateState)
    new StandardMapView[PlanId, Plan]:
      override def keySet =
        toPlanTemplateState.values.view.flatMap: planTemplateState =>
          planTemplateState.planIds.view.map(planTemplateState.id / _)
        .toSet

      final def get(planId: PlanId) =
        toPlanTemplateState.get(planId.planTemplateId).flatMap(_.toPlan.get(planId.planKey))

  @TestOnly
  final def templateToKeyToPlan: MapView[PlanTemplateId, Map[PlanKey, Plan]] =
    val toPlanTemplateState = keyTo(PlanTemplateState)
    new StandardMapView[PlanTemplateId, Map[PlanKey, Plan]]:
      override def keySet = toPlanTemplateState.keySet

      def get(planTemplateId: PlanTemplateId) =
        toPlanTemplateState.get(planTemplateId).map(_.toPlan)
