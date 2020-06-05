package js7.proxy.javaapi.data

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.order.{Order, OrderId}

@javaApi
final case class JOrder(underlying: Order[Order.State])
extends JJsonable[JOrder]
{
  protected type Underlying = Order[Order.State]

  protected def companion = JOrder

  def id: OrderId =
    underlying.id

  lazy val workflowId: JWorkflowId =
    JWorkflowId(underlying.workflowId)
}

@javaApi
object JOrder extends JJsonable.Companion[JOrder]
{
  override def fromJson(jsonString: String): VEither[Problem, JOrder] =
    super.fromJson(jsonString)

  val jsonEncoder = Order.jsonEncoder
  val jsonDecoder = Order.jsonDecoder
}
