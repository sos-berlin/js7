package js7.data_for_java.order

import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.data.order.OrderObstacle

sealed trait JOrderObstacle

object JOrderObstacle
{
  def apply(orderObstacle: OrderObstacle): JOrderObstacle =
    orderObstacle match {
      case o: OrderObstacle.WaitingForTime => WaitingForTime(o)
      case OrderObstacle.WaitingForCommand => WaitingForCommand
    }

  final case object WaitingForCommand
  extends JOrderObstacle

  final case class WaitingForTime(asScala: OrderObstacle.WaitingForTime)
  extends JOrderObstacle {
    def instant = asScala.timestamp.toInstant
  }
}
