package js7.data.plan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticeId
import js7.data.plan.PlanId.*

/** Identifies a 'Plan', a thought thing which exists only as this `PlanId`. */
final case class PlanId(planTemplateId: PlanTemplateId, planKey: PlanKey):

  def isGlobal: Boolean =
    this == Global

  def noticeId: NoticeId =
    NoticeId.planned(this)

  override def toString =
    s"Plan:$shortString"

  def shortString =
    s"${planTemplateId.string}${planKey.string.nonEmpty ?? s"/${planKey.string}"}"


object PlanId:

  val Global: PlanId = PlanId(PlanTemplateId.Global, PlanKey.Global)

  given Encoder[PlanId] =
    case PlanId(planTemplateId, planKey) => Json.arr(planTemplateId.asJson, planKey.asJson)

  given Decoder[PlanId] = c =>
    c.as[(PlanTemplateId, PlanKey)].map:
      PlanId.apply.tupled
