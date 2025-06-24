package js7.data.plan

import io.circe.derivation.ConfiguredEncoder
import io.circe.{Codec, Decoder, Encoder}
import js7.base.metering.CallMeter
import js7.base.problem.Checked
import js7.base.time.ScalaTime.ZeroDuration
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.item.{ItemRevision, UnsignedSimpleItem}
import js7.data.plan.PlanSchema.*
import js7.data.value.expression.Expression.{Not, exprFun}
import js7.data.value.expression.{ExprFunction, Scope}
import js7.data.value.{NamedValues, StringValue}
import org.jetbrains.annotations.TestOnly

/** Item for (daily) plans.
  **
  * @param unknownPlanIsOpenFunction A function expression with a PlanId as argument,
  *                             returns true when the named unknown Plan should be open.
  * #@param startOffset When the plan starts (for example 6h for 06:00 local time)
  * #@param lifetime How long a daily plan is kept after the day is over
  */
final case class PlanSchema(
  id: PlanSchemaId,
  unknownPlanIsOpenFunction: ExprFunction,
  namedValues: NamedValues = NamedValues.empty,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:

  protected type Self = PlanSchema

  val companion: PlanSchema.type = PlanSchema

  def path: PlanSchemaId = id

  def estimatedSnapshotSize: Int =
    (!isGlobal).toInt

  def toSnapshotStream: fs2.Stream[fs2.Pure, PlanSchema] =
    fs2.Stream.fromOption:
      !isGlobal ? this

  def toInitialItemState: PlanSchemaState =
    PlanSchemaState(
      this,
      finishedPlanRetentionPeriod = ZeroDuration,
      namedValues = NamedValues.empty,
      toPlan = Map.empty)

  def isGlobal: Boolean =
    id.isGlobal

  def rename(id: PlanSchemaId): PlanSchema =
    copy(id = id)

  def withRevision(revision: Option[ItemRevision]): PlanSchema =
    copy(itemRevision = revision)

  private[plan] def evalUnknownPlanIsOpen(planKey: PlanKey, scope: Scope): Checked[Boolean] =
    meterUnknownPlanIsOpenFunction:
      unknownPlanIsOpenFunction.eval(StringValue(planKey.string))(using scope)
        .flatMap(_.asBoolean)


object PlanSchema extends UnsignedSimpleItem.Companion[PlanSchema]:

  type Key = PlanSchemaId
  type ItemState = PlanSchemaState

  val Key: PlanSchemaId.type = PlanSchemaId
  val Path: PlanSchemaId.type = PlanSchemaId
  val cls: Class[PlanSchema] = classOf[PlanSchema]

  final val EachUnknownPlanIsOpen: ExprFunction =
    exprFun"planKey => true"

  final val EachUnknownPlanIsClosed: ExprFunction =
    exprFun"planKey => false"

  final val Global: PlanSchema =
    PlanSchema(PlanSchemaId.Global, unknownPlanIsOpenFunction = EachUnknownPlanIsOpen)

  /** A PlanSchema for JOC-style daily plan OrderIds. */
  @TestOnly
  def joc(id: PlanSchemaId): PlanSchema =
    PlanSchema(
      id,
      unknownPlanIsOpenFunction = exprFun"day => $$day >= $$unknownPlansAreOpenFrom",
      Map("unknownPlansAreOpenFrom" -> StringValue.empty))

  /** A PlanSchema for weekly Plan Orders "#YYYYwWW#...". */
  @TestOnly
  def weekly(id: PlanSchemaId): PlanSchema =
    PlanSchema(
      id,
      unknownPlanIsOpenFunction = exprFun"week => $$week >= $$openingWeek",
      Map("openingWeek" -> StringValue.empty))

  private val meterUnknownPlanIsOpenFunction = CallMeter("PlanSchema.unknownPlanIsOpenFunction")

  override given jsonEncoder: Encoder.AsObject[PlanSchema] = ConfiguredEncoder.derive()

  override given jsonDecoder: Decoder[PlanSchema] = c =>
    for
      id <- c.get[PlanSchemaId]("id")
      planIsOpenFunction <-
        c.get[ExprFunction]("unknownPlanIsOpenFunction")
          .orElse: // COMPATIBLE with 2.7.4
            c.get[ExprFunction]("unknownPlanIsClosedFunction").map: fun =>
              fun.copy(expression = Not(fun.expression))
      namedValues <- c.getOrElse[NamedValues]("namedValues")(NamedValues.empty)
      itemRevision <- c.get[Option[ItemRevision]]("itemRevision")
    yield
      PlanSchema(id, planIsOpenFunction, namedValues, itemRevision)

  given jsonCodec: Codec.AsObject[PlanSchema] = Codec.AsObject.from(jsonDecoder, jsonEncoder)
