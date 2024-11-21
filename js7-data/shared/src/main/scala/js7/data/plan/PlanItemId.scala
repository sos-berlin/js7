package js7.data.plan

import js7.base.generic.GenericString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.UnsignedSimpleItemPath
import org.jetbrains.annotations.TestOnly

final case class PlanItemId private(string: String)
  extends GenericString, UnsignedSimpleItemPath:

  protected type Self = PlanItemId

  val companion: PlanItemId.type = PlanItemId

  def /(planKey: PlanKey): PlanId =
    PlanId(this, planKey)

  @TestOnly
  def /(planKey: String): PlanId =
    PlanId(this, PlanKey(planKey))


object PlanItemId extends UnsignedSimpleItemPath.Companion[PlanItemId]:

  type Item = PlanItem

  override protected val isReserved = Set("Global")

  val Global: PlanItemId = new PlanItemId("Global")

  protected def unchecked(string: String) =
    new PlanItemId(string)
