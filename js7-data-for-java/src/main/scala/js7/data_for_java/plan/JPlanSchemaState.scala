package js7.data_for_java.plan

import javax.annotation.Nonnull
import js7.data.order.OrderId
import js7.data.plan.{PlanSchemaId, PlanSchemaState}
import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import scala.jdk.CollectionConverters.*

final case class JPlanSchemaState(asScala: PlanSchemaState)
extends JavaWrapper:

  type AsScala = PlanSchemaState

  @Nonnull
  def id: PlanSchemaId =
    asScala.id

  @Nonnull
  def item: JPlanSchema =
    JPlanSchema(asScala.item)

  @Nonnull
  def orderIds: java.lang.Iterable[OrderId] =
    asScala.orderIds.asJava

  @Nonnull
  def namedValues: java.util.Map[String, Value] =
    asScala.namedValues.asJava
