package js7.data.plan

import cats.syntax.traverse.*
import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import io.circe.{Codec, Decoder, Encoder}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import js7.data.plan.PlanSchema.*
import js7.data.value.expression.Expression.MissingConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.{ExprFunction, Expression, Scope}
import js7.data.value.{NamedValues, StringValue}
import org.jetbrains.annotations.TestOnly

/** Item for (daily) plans.
  *
  * Live states of a Plan
  *
  * <dl>
  *   <dt>Dead or non-existent closed
  *   <dd>"Dead Plan" is synonymous to "non-existing closed Plan".
  *   <br>A dead Plan is a closed Plan without Orders (and thus without Notices)
  *   <p>
  *     There is no way to add an Order to a dead Plan.
  *   <p>
  *     A dead Plan
  *     <ul>
  *     <li>has never exist, or
  *     <li>has been closed and all Orders (and thus Notices) have been deleted.
  *     </ul>
  *
  *   <dt>Closed
  *   <dd>iff `planIsClosedFunction` returns true for the PlanId.
  *   <p>
  *     A closed Plan does not accept external Orders, i.e. Order fed via web service or FileWatch.
  *     But as long as the closed Plan is not dead (i.e. contains an Order), Orders may be added
  *     via instruction: AddOrder, Fork, ForkList.
  *     (For Fork and ForkList, the Plan cannot be dead,
  *     because the forking Order is in the same Plan).
  *   <p>
  *     When the last Order of a closed Plan has been deleted,
  *     the Engine deletes all Notices of the Plan, too.
  *     The Plan gets empty and thus non-existent.
  *     An empty closed Plan is equal to a non-existent closed Plan and called dead.
  * </dl>
  *
  * @param planIsClosedFunction A function expression with a PlanId as argument,
  *                             returns true when the corresponding Plan is closed.
  * #@param startOffset When the plan starts (for example 6h for 06:00 local time)
  * #@param lifetime How long a daily plan is kept after the day is over
  */
final case class PlanSchema(
  id: PlanSchemaId,
  planKeyExpr: Expression,
  planIsClosedFunction: Option[ExprFunction] = None,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:

  protected type Self = PlanSchema

  val companion: PlanSchema.type = PlanSchema

  def path: PlanSchemaId = id

  def toSnapshotStream: fs2.Stream[fs2.Pure, PlanSchema] =
    fs2.Stream.fromOption:
      !isGlobal ? this

  def toInitialItemState: PlanSchemaState =
    PlanSchemaState(this, namedValues = NamedValues.empty, toPlan = Map.empty)

  def isGlobal: Boolean =
    this eq Global

  def rename(id: PlanSchemaId): PlanSchema =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): PlanSchema =
    copy(itemRevision = revision)

  /** @param scope is expected to contain the Order Scope.
    * @return None iff `planKeyExpr` expression evaluates to MissingValue (no match). */
  def evalOrderToPlanId(scope: Scope): Checked[Option[PlanId]] =
    evalPlanKeyExpr(scope)
      .map(_.map(PlanId(id, _)))

  /** @param scope is expected to contain the Order Scope.
    * @return None iff `planKeyExpr` expression evaluates to MissingValue (no match). */
  private def evalPlanKeyExpr(scope: Scope): Checked[Option[PlanKey]] =
    planKeyExpr.eval(using scope).flatMap:
      _.missingToNone.traverse:
        _.toStringValueString.flatMap(PlanKey.checked)

  private[plan] def isClosed(planKey: PlanKey, scope: Scope): Checked[Boolean] =
    planIsClosedFunction
      .fold_(Right(false), function =>
        val args = StringValue(planKey.string) :: Nil
        function.eval(args)(using scope)
          .flatMap(_.asBoolean))


object PlanSchema extends UnsignedSimpleItem.Companion[PlanSchema]:

  type ItemState = PlanSchemaState

  val Global: PlanSchema =
    PlanSchema(
      PlanSchemaId.Global,
      // planKeyExpr must not match, despite Global is used as the fallback PlanSchema
      planKeyExpr = MissingConstant)

  /** A PlanSchema for JOC-style daily plan OrderIds. */
  @TestOnly
  def joc(id: PlanSchemaId, planIsClosedFunction: Option[ExprFunction] = None): PlanSchema =
    PlanSchema(
      id,
      planKeyExpr = PlanKey.jocPlanKeyExpr,
      planIsClosedFunction = planIsClosedFunction)

  /** A PlanSchema for weekly Plan Orders "#YYYYwWW#...". */
  @TestOnly
  def weekly(id: PlanSchemaId): PlanSchema =
    PlanSchema(
      id,
      planKeyExpr = expr("match(orderId, '#([0-9]{4}w[0-9]{2})#.*', '$1') ?"))

  type Key = PlanSchemaId

  val Key: PlanSchemaId.type = PlanSchemaId
  val Path: PlanSchemaId.type = PlanSchemaId
  val cls: Class[PlanSchema] = classOf[PlanSchema]

  override given jsonEncoder: Encoder.AsObject[PlanSchema] = ConfiguredEncoder.derive()
  override given jsonDecoder: Decoder[PlanSchema] = ConfiguredDecoder.derive(useDefaults = true)
  given jsonCodec: Codec.AsObject[PlanSchema] = Codec.AsObject.from(jsonDecoder, jsonEncoder)
