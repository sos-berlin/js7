package js7.executor.forjava.internal

import javax.annotation.Nonnull
import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.order.JOrder
import js7.data_for_java.workflow.JWorkflow
import js7.executor.internal.InternalJob.OrderContext
import scala.jdk.CollectionConverters._

trait JavaOrderContext extends JavaWrapper
{
  type AsScala = OrderContext

  @Nonnull
  def asScala: OrderContext

  @Nonnull
  final lazy val order =
    JOrder(asScala.order)

  @Nonnull
  final lazy val workflow =
    JWorkflow(asScala.workflow)

  @Nonnull
  final lazy val arguments: java.util.Map[String, Value] =
    asScala.arguments.asJava
}
