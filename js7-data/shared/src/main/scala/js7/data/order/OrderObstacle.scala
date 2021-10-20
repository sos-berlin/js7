package js7.data.order

import js7.base.time.Timestamp

sealed trait OrderObstacle

object OrderObstacle
{
  val waitingForCommand: OrderObstacle =
    WaitingForCommand

  def waitingForTime(timestamp: Timestamp): OrderObstacle =
    WaitingForTime(timestamp)

  final case object WaitingForCommand
  extends OrderObstacle

  final case class WaitingForTime(timestamp: Timestamp)
  extends OrderObstacle
}
