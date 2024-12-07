package js7.data.plan

import cats.syntax.traverse.*
import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import io.circe.{Codec, Decoder, Encoder}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import js7.data.plan.PlanTemplate.*
import js7.data.value.expression.Expression.MissingConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.{ExprFunction, Expression, Scope}
import js7.data.value.{NamedValues, StringValue}
import org.jetbrains.annotations.TestOnly

/** Item for (daily) plans.
  *
  * #@param startOffset When the plan starts (for example 6h for 06:00 local time)
  * #@param lifetime How long a daily plan is kept after the day is over
  */
final case class PlanTemplate(
  id: PlanTemplateId,
  orderToPlanKey: Expression,
  planIsOpenFunction: Option[ExprFunction] = None,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:

  protected type Self = PlanTemplate

  val companion: PlanTemplate.type = PlanTemplate

  def path: PlanTemplateId = id

  def toSnapshotStream: fs2.Stream[fs2.Pure, PlanTemplate] =
    fs2.Stream.fromOption:
      !isGlobal ? this

  def toInitialItemState: PlanTemplateState =
    PlanTemplateState(this, namedValues = NamedValues.empty, toOrderPlan = Map.empty)

  def isGlobal: Boolean =
    this eq Global

  def rename(id: PlanTemplateId): PlanTemplate =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): PlanTemplate =
    copy(itemRevision = revision)

  /** @param scope is expected to contain the Order Scope.
    * @return None iff `orderToPlanKey` expression evaluates to MissingValue (no match). */
  def evalOrderToPlanId(scope: Scope): Checked[Option[PlanId]] =
    evalOrderToPlanKey(scope)
      .map(_.map(PlanId(id, _)))

  /** @param scope is expected to contain the Order Scope.
    * @return None iff `orderToPlanKey` expression evaluates to MissingValue (no match). */
  private def evalOrderToPlanKey(scope: Scope): Checked[Option[PlanKey]] =
    orderToPlanKey.eval(using scope).flatMap:
      _.missingToNone.traverse:
        _.toStringValueString.flatMap(PlanKey.checked)

  private[plan] def isOpen(planKey: PlanKey, scope: Scope): Checked[Boolean] =
    val args = StringValue(planKey.string) :: Nil
    planIsOpenFunction
      .fold_(Right(true), function =>
        function.eval(args)(using scope)
          .flatMap(_.asBoolean))


object PlanTemplate extends UnsignedSimpleItem.Companion[PlanTemplate]:

  type ItemState = PlanTemplateState

  val Global: PlanTemplate =
    PlanTemplate(
      PlanTemplateId.Global,
      // orderToPlanKey must not match, despite Global is used as the fallback PlanTemplate
      orderToPlanKey = MissingConstant)

  /** A PlanTemplate for JOC-style daily plan OrderIds. */
  def joc(id: PlanTemplateId, planIsOpenFunction: Option[ExprFunction] = None): PlanTemplate =
    PlanTemplate(
      id,
      orderToPlanKey = PlanKey.jocOrderToPlanKey,
      planIsOpenFunction = planIsOpenFunction)

  /** A PlanTemplate for weekly Plan Orders "#YYYYwWW#...". */
  @TestOnly
  def weekly(id: PlanTemplateId): PlanTemplate =
    PlanTemplate(
      id,
      orderToPlanKey = expr("match(orderId, '#([0-9]{4}w[0-9]{2})#.*', '$1') ?"))

  type Key = PlanTemplateId

  val Key: PlanTemplateId.type = PlanTemplateId
  val Path: PlanTemplateId.type = PlanTemplateId
  val cls: Class[PlanTemplate] = classOf[PlanTemplate]

  override given jsonEncoder: Encoder.AsObject[PlanTemplate] = ConfiguredEncoder.derive()
  override given jsonDecoder: Decoder[PlanTemplate] = ConfiguredDecoder.derive(useDefaults = true)
  given jsonCodec: Codec.AsObject[PlanTemplate] = Codec.AsObject.from(jsonDecoder, jsonEncoder)
