package js7.data.plan

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import scala.annotation.static

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
  @static final val Global: PlanKey = new PlanKey("Global")

  protected def unchecked(string: String) =
    new PlanKey(string)

  @javaApi @throws[RuntimeException]
  def of(planKey: String): PlanKey =
    mayThrow(planKey)

  given Ordering[PlanKey] = GenericString.ordering
