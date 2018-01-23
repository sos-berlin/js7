package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.order.OrderEvent.OrderOffered
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Instructions.ifProcessedThenOrderMoved
import com.sos.jobscheduler.data.workflow.{EventInstruction, OrderContext}
import io.circe.generic.JsonCodec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Offer(orderId: OrderId, timeout: FiniteDuration) extends EventInstruction
{
  def toEvent(order: Order[Order.State], context: OrderContext) =
    order.ifState[Order.Idle].map(
      _.id <-: OrderOffered(orderId, Timestamp.now + timeout))
    .orElse(
      ifProcessedThenOrderMoved(order, context))

  override def toString = s"offer orderId=$orderId, timeout=$timeout"
}

object Offer {
  intelliJuseImport(FiniteDurationJsonEncoder)
}
