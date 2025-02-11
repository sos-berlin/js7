package js7.data_for_java.plan

import io.vavr.control.Either as VEither
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.item.ItemRevision
import js7.data.plan.{PlanSchema, PlanSchemaId}
import js7.data.value.Value
import js7.data.value.expression.ExprFunction
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import js7.data_for_java.value.JExpression
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final case class JPlanSchema(asScala: PlanSchema)
extends JJsonable[JPlanSchema], JUnsignedSimpleItem:

  type AsScala = PlanSchema
  protected def companion = JPlanSchema

  @Nonnull
  def path: PlanSchemaId =
    asScala.path

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]): JPlanSchema =
    copy(asScala.withRevision(revision.toScala))

  @Nonnull def id: PlanSchemaId =
    asScala.id

  @Nonnull def planKeyExpr: JExpression =
    JExpression(asScala.planKeyExpr)

  @Nonnull def planIsClosedFunction: Optional[ExprFunction] =
    asScala.planIsClosedFunction.toJava

  @Nonnull def namedValues: java.util.Map[String, Value] =
    asScala.namedValues.asJava


object JPlanSchema extends JJsonable.Companion[JPlanSchema]:
  type AsScala = PlanSchema

  @Nonnull
  def of(
    @Nonnull id: PlanSchemaId,
    @Nonnull planKeyExpr: JExpression,
    @Nonnull planIsClosedFunction: Optional[ExprFunction],
    @Nonnull namedValues: java.util.Map[String, Value @Nonnull])
  : JPlanSchema =
    JPlanSchema:
      PlanSchema(id, planKeyExpr.asScala, planIsClosedFunction.toScala, namedValues.asScala.toMap)

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JPlanSchema] =
    super.fromJson(jsonString)

  protected def jsonEncoder = PlanSchema.jsonCodec
  protected def jsonDecoder = PlanSchema.jsonCodec
