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
final case class PlanItem(
  id: PlanItemId,
  orderToPlanKey: Expression,
  itemRevision: Option[ItemRevision] = None)
extends
  UnsignedSimpleItem, UnsignedSimpleItemState, TrivialItemState[PlanItem]:

  protected type Self = PlanItem

  val companion: PlanItem.type = PlanItem

  def path: PlanItemId = id

  def rename(id: PlanItemId): PlanItem =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): PlanItem =
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


object PlanItem extends
  UnsignedSimpleItem.Companion[PlanItem],
  UnsignedSimpleItemState.Companion[PlanItem],
  TrivialItemState.Companion[PlanItem]:

  /** A PlanItem for JOC-style OrderIds. */
  def joc(id: PlanItemId): PlanItem =
    PlanItem(
      id,
      orderToPlanKey = PlanKey.jocOrderToPlanKey)

  type Key = PlanItemId

  val Key: PlanItemId.type = PlanItemId
  val Path: PlanItemId.type = PlanItemId
  val cls: Class[PlanItem] = classOf[PlanItem]

  given jsonCodec: Codec.AsObject[PlanItem] = deriveConfiguredCodec
