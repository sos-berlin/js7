package js7.data.plan

import cats.syntax.traverse.*
import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.{ItemRevision, TrivialItemState, UnsignedSimpleItem, UnsignedSimpleItemState}
import js7.data.value.expression.{Expression, Scope}

/** Item for (daily) plans.
  *
  * #@param startOffset When the plan starts (for example 6h for 06:00 local time)
  * #@param lifetime How long a daily plan is kept after the day is over
  */
final case class PlanTemplate(
  id: PlanTemplateId,
  orderToPlanKey: Expression,
  itemRevision: Option[ItemRevision] = None)
extends
  UnsignedSimpleItem, UnsignedSimpleItemState, TrivialItemState[PlanTemplate]:

  protected type Self = PlanTemplate

  val companion: PlanTemplate.type = PlanTemplate

  def path: PlanTemplateId = id

  def rename(id: PlanTemplateId): PlanTemplate =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): PlanTemplate =
    copy(itemRevision = revision)

  /** @param scope is expected to contain the Order Scope.
    * @return None iff `orderToPlanKey` expression evaluates to MissingValue (no match). */
  def evalOrderToPlanId(scope: Scope): Option[Checked[PlanId]] =
    evalOrderToPlanKey(scope)
      .map(_.map:
        PlanId(id, _))

  /** @param scope is expected to contain the Order Scope.
    * @return None iff `orderToPlanKey` expression evaluates to MissingValue (no match). */
  private def evalOrderToPlanKey(scope: Scope): Option[Checked[PlanKey]] =
    orderToPlanKey.eval(using scope)
      .traverse(_.missingToNone)
      .map:
        _.flatMap(_.toStringValueString)
          .flatMap(PlanKey.checked)


object PlanTemplate extends
  UnsignedSimpleItem.Companion[PlanTemplate],
  UnsignedSimpleItemState.Companion[PlanTemplate],
  TrivialItemState.Companion[PlanTemplate]:

  /** A PlanTemplate for JOC-style OrderIds. */
  def joc(id: PlanTemplateId): PlanTemplate =
    PlanTemplate(
      id,
      orderToPlanKey = PlanKey.jocOrderToPlanKey)

  type Key = PlanTemplateId

  val Key: PlanTemplateId.type = PlanTemplateId
  val Path: PlanTemplateId.type = PlanTemplateId
  val cls: Class[PlanTemplate] = classOf[PlanTemplate]

  given jsonCodec: Codec.AsObject[PlanTemplate] = deriveConfiguredCodec