package js7.data.plan

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.plan.PlanId.*

/** Identifies a 'Plan', a thought thing which exists only as this `PlanId`. */
final case class PlanId(planItemId: PlanItemId, planKey: PlanKey):

  def isGlobal: Boolean =
    this == Global

  def maybe: Option[this.type] =
    !isGlobal ? this

  override def toString =
    s"Plan:$shortString"

  def shortString =
    s"${planItemId.string}${planKey.string.nonEmpty ?? s"/${planKey.string}"}"


object PlanId:

  val Global: PlanId = PlanId(PlanItemId.Global, PlanKey.Global)

  given Encoder[PlanId] =
    case Global => Json.Null
    case PlanId(planItemId, planKey) => Json.arr(planItemId.string.asJson, planKey.string.asJson)


  given Decoder[PlanId] = c =>
    if c.value.isNull then
      Right(PlanId.Global)
    else
      c.as[(PlanItemId, PlanKey)].flatMap:
        case (Global.planItemId, Global.planKey) =>
          Right(Global) // Reuse Memory
        case (Global.planItemId, _) =>
          Left(DecodingFailure("Invalid Global PlanId", c.history))
        case (planItemId, planKey) =>
          Right(PlanId(planItemId, planKey))
