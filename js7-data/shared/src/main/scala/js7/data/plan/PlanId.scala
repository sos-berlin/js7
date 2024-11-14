package js7.data.plan

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveCodecWithDefaults
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

  given Codec[PlanId] = deriveCodecWithDefaults[PlanId]
