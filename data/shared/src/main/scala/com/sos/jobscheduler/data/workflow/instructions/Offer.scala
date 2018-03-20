package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.generic.JsonCodec
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Offer(orderId: OrderId, timeout: FiniteDuration) extends Instruction
{
  override def toString = s"offer orderId=$orderId, timeout=$timeout"
}

object Offer {
  intelliJuseImport(FiniteDurationJsonEncoder)
}
