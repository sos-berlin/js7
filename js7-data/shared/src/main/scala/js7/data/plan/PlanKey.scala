package js7.data.plan

import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
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

  /** This name is internally used only.
    * It should serialize to None. */
  final val Global: PlanKey = new PlanKey("")

  val jocPlanKeyExpr: Expression =
    expr""" match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1') ? """

  protected def unchecked(string: String) =
    new PlanKey(string)

  override def checked(string: String): Checked[PlanKey] =
    // "Global" should never occur. Instead, a global PlanKey is omitted.
    // This check may no longer necessary after development.
    if string == Global.string then
      Left(Problem.pure("Invalid PlanKey:Global"))
    else
      super.checked(string)

  given Ordering[PlanKey] = GenericString.ordering
