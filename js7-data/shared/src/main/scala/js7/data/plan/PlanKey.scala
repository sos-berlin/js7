package js7.data.plan

import js7.base.generic.GenericString
import js7.base.problem.{Checked, Problem}
import js7.data.value.expression.Expression
import js7.data.value.expression.ExpressionParser.expr

/** Identifies a Plan of a PlanItem.
  * @see `PlanId` which identifies a Plan globally.
  */

final case class PlanKey private(string: String) extends GenericString:

  override def toString = s"Plan:$string"


object PlanKey extends GenericString.NameValidating[PlanKey]:

  /** This name is internally used only.
    * It should serialize to None. */
  val Global: PlanKey = new PlanKey("")

  val jocOrderToPlanKey: Expression =
    expr("match($js7OrderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$1') ?")

  protected def unchecked(string: String) =
    new PlanKey(string)

  override def checked(string: String): Checked[PlanKey] =
    if string == Global.string then
      Left(Problem.pure("Invalid PlanKey"))
    else
      super.checked(string)
