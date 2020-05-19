package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.order.{Order, OrderId}
import io.vavr.control.{Either => VEither}

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
