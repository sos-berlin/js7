package js7.data.workflow.instructions

import io.circe.generic.semiauto.deriveCodec
import js7.data.order.OrderId
import js7.data.source.SourcePos
import js7.data.workflow.Instruction

/**
  * @author Joacim Zschimmer
  */
final case class AwaitOrder(orderId: OrderId, sourcePos: Option[SourcePos] = None)
extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = s"await orderId=$orderId$sourcePosToString"
}

object AwaitOrder {
  implicit val jsonCodec = deriveCodec[AwaitOrder]
}
