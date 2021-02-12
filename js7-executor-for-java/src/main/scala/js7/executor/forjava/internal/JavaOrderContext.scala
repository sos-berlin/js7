package js7.executor.forjava.internal

import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import js7.data_for_java.order.JOrder
import js7.data_for_java.workflow.JWorkflow
import js7.executor.internal.InternalJob.OrderContext
import scala.jdk.CollectionConverters._

trait JavaOrderContext extends JavaWrapper
{
  type AsScala = OrderContext

  def asScala: OrderContext

  final lazy val order =
    JOrder(asScala.order)

  final lazy val workflow =
    JWorkflow(asScala.workflow)

  final lazy val arguments: java.util.Map[String, Value] =
    asScala.arguments.asJava
}
