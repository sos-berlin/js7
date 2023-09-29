package js7.data_for_java.order

import java.time.Instant
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.data.order.OrderObstacle

sealed trait JOrderObstacle

object JOrderObstacle:
  val toScalaClass = Map[Class[? <: JOrderObstacle], Class[? <: OrderObstacle]](
    WaitingForCommand.getClass -> classOf[OrderObstacle.WaitingForTime],
    classOf[WaitingForTime] -> classOf[OrderObstacle.WaitingForTime])

  def apply(orderObstacle: OrderObstacle): JOrderObstacle =
    orderObstacle match
      case o: OrderObstacle.WaitingForAdmission => WaitingForAdmission(o)
      case o: OrderObstacle.WaitingForOtherTime => WaitingForOtherTime(o)
      case OrderObstacle.WaitingForCommand => WaitingForCommand
      case OrderObstacle.JobParallelismLimitReached => jobParallelismLimitReached
      case OrderObstacle.WorkflowSuspended => workflowSuspended

  type WaitingForCommand = WaitingForCommand.type
  case object WaitingForCommand
  extends JOrderObstacle

  sealed trait WaitingForTime extends JOrderObstacle:
    def asScala: OrderObstacle.WaitingForTime

    final def until: Instant =
      asScala.until.toInstant

  final case class WaitingForOtherTime(asScala: OrderObstacle.WaitingForOtherTime)
  extends WaitingForTime

  final case class WaitingForAdmission(asScala: OrderObstacle.WaitingForTime)
  extends WaitingForTime

  val jobParallelismLimitReached =
    JobParallelismLimitReached(OrderObstacle.JobParallelismLimitReached)

  final case class JobParallelismLimitReached(asScala: OrderObstacle.JobParallelismLimitReached.type)
  extends JOrderObstacle

  val workflowSuspended = WorkflowSuspended(OrderObstacle.WorkflowSuspended)

  final case class WorkflowSuspended(asScala: OrderObstacle.WorkflowSuspended)
  extends JOrderObstacle
