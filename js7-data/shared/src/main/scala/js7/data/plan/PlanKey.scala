package js7.data.plan

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.expr

/** Identifies a Plan of a PlanSchema.
  * <p>
  *   For a DailyPlan, PlanKey is the date "2024-11-27" of the DailyPlan.
  * @see `PlanId` which identifies a Plan globally.
  */

final case class PlanKey private(string: String) extends GenericString:

  override def toString = s"Plan:$string"


object PlanKey extends GenericString.NameValidating[PlanKey]:

  /** Used for the one and only Plan in the predefined global PlanSchema.
    */
  final val Global: PlanKey = new PlanKey("Global")

  val jocPlanKeyExpr: Expression =
    expr""" match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1') ? """

  protected def unchecked(string: String) =
    new PlanKey(string)

  @javaApi @throws[RuntimeException]
  def of(planKey: String): PlanKey =
    mayThrow(planKey)

  given Ordering[PlanKey] = GenericString.ordering
