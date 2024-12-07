package js7.data.plan

import js7.base.generic.GenericString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.UnsignedSimpleItemPath
import js7.data.plan.PlanTemplateId.Global
import org.jetbrains.annotations.TestOnly

final case class PlanTemplateId private(string: String)
  extends GenericString, UnsignedSimpleItemPath:

  protected type Self = PlanTemplateId

  val companion: PlanTemplateId.type = PlanTemplateId

  def isGlobal: Boolean =
    this == Global

  def /(planKey: PlanKey): PlanId =
    PlanId(this, planKey)

  @TestOnly
  def /(planKey: String): PlanId =
    PlanId(this, PlanKey(planKey))


object PlanTemplateId extends UnsignedSimpleItemPath.Companion[PlanTemplateId]:

  type Item = PlanTemplate

  override protected val isReserved = Set("Global")

  val Global: PlanTemplateId = new PlanTemplateId("Global")

  protected def unchecked(string: String) =
    new PlanTemplateId(string)
