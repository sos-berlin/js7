package js7.data.plan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.orderingBy
import js7.data.board.{BoardNoticeKey, BoardPath, NoticeId, NoticeKey, PlannedBoardId, PlannedNoticeKey}
import js7.data.plan.PlanId.*

/** Identifies a 'Plan', a thought thing which exists only as this `PlanId`. */
final case class PlanId(planSchemaId: PlanSchemaId, planKey: PlanKey):

  def isGlobal: Boolean =
    this == Global

  def /(boardNoticeKey: BoardNoticeKey): NoticeId =
    NoticeId(this, boardNoticeKey)

  def /(boardPath: BoardPath): PlannedBoardId =
    PlannedBoardId(this, boardPath)

  def /(noticeKey: NoticeKey): PlannedNoticeKey =
    PlannedNoticeKey(this, noticeKey)

  def checked: Checked[this.type] =
    if planSchemaId.isGlobal && planKey != PlanKey.Global then
      Left(Problem:
        s"Invalid $toString: The only PlanKey in the global PlanSchema is ${PlanKey.Global}))")
    else
      Right(this)

  override def toString =
    s"Plan:$shortString"

  def shortString: String =
    if isGlobal then
      "Global"
    else
      s"${planSchemaId.string}╱${planKey.string}"


object PlanId:

  final val Global: PlanId =
    PlanSchemaId.Global / PlanKey.Global

  given Ordering[PlanId] = orderingBy(_.planSchemaId, _.planKey)

  given Encoder[PlanId] =
    case PlanId(planSchemaId, planKey) => Json.arr(planSchemaId.asJson, planKey.asJson)

  given Decoder[PlanId] = c =>
    c.as[(PlanSchemaId, PlanKey)].flatMap: (planSchemaId, planKey) =>
      PlanId(planSchemaId, planKey).checked.toDecoderResult(c.history)
