package js7.data.workflow.instructions

import io.circe.generic.JsonCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.order.OrderId
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Offer(orderId: OrderId, timeout: FiniteDuration, sourcePos: Option[SourcePos] = None) extends Instruction
{
  def withoutSourcePos = copy(sourcePos = None)

  override def toString = s"offer orderId=$orderId, timeout=$timeout$sourcePosToString"
}

object Offer {
  intelliJuseImport(FiniteDurationJsonEncoder)
}
