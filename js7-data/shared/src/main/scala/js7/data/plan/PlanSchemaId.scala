package js7.data.plan

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.UnsignedSimpleItemPath
import js7.data.plan.PlanSchemaId.Global
import org.jetbrains.annotations.TestOnly

/** A template for Plans-
  *
  * "DailyPlan", daily plan, is a usual PlanSchemaId.
  */
final case class PlanSchemaId private(string: String)
  extends GenericString, UnsignedSimpleItemPath:

  protected type Self = PlanSchemaId

  val companion: PlanSchemaId.type = PlanSchemaId

  def isGlobal: Boolean =
    this == Global

  def /(planKey: PlanKey): PlanId =
    PlanId(this, planKey)

  @TestOnly @throws[RuntimeException]
  def /(planKey: String): PlanId =
    PlanId(this, PlanKey(planKey))


object PlanSchemaId extends UnsignedSimpleItemPath.Companion[PlanSchemaId]:

  type Item = PlanSchema

  override protected val isReserved = Set("Global")

  final val Global: PlanSchemaId = new PlanSchemaId("Global")

  protected def unchecked(string: String) =
    new PlanSchemaId(string)

  @javaApi @throws[RuntimeException]
  def of(planSchemaId: String): PlanSchemaId =
    checked(planSchemaId).orThrow