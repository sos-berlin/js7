package js7.data.plan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.plan.PlanId.*

/** Identifies a 'Plan', a thought thing which exists only as this `PlanId`. */
final case class PlanId(planItemId: PlanItemId, planKey: PlanKey):

  def isGlobal: Boolean =
    this == Global

  override def toString =
    s"Plan:$shortString"

  def shortString =
    s"${planItemId.string}${planKey.string.nonEmpty ?? s"/${planKey.string}"}"


object PlanId:

  val Global: PlanId = PlanId(PlanItemId.Global, PlanKey.Global)

  given Encoder[PlanId] =
    case PlanId(planItemId, planKey) => Json.arr(planItemId.asJson, planKey.asJson)

  given Decoder[PlanId] = c =>
    c.as[(PlanItemId, PlanKey)].map:
      PlanId.apply.tupled
