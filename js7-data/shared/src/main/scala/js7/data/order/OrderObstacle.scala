package js7.data.order

import js7.base.time.Timestamp

sealed trait OrderObstacle


object OrderObstacle:
  val waitingForCommand: OrderObstacle =
    WaitingForCommand

  def waitingForOtherTime(until: Timestamp): OrderObstacle =
    WaitingForOtherTime(until)

  def waitingForAdmmission(until: Timestamp): OrderObstacle =
    WaitingForAdmission(until)

  val agentProcessLimitReached: OrderObstacle =
    AgentProcessLimitReached

  val jobProcessLimitReached: OrderObstacle =
    JobProcessLimitReached

  case object WaitingForCommand extends OrderObstacle

  sealed trait WaitingForTime extends OrderObstacle:
    def until: Timestamp

  final case class WaitingForAdmission(until: Timestamp)
  extends WaitingForTime

  final case class WaitingForOtherTime(until: Timestamp)
  extends WaitingForTime

  case object AgentProcessLimitReached
  extends OrderObstacle

  case object JobProcessLimitReached
  extends OrderObstacle

  type WorkflowSuspended = WorkflowSuspended.type
  case object WorkflowSuspended
  extends OrderObstacle
