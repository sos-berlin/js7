package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AwaitOrder(orderId: OrderId, sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = s"await orderId=$orderId"
}
