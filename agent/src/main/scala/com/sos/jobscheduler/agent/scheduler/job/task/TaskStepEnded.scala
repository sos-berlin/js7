package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.order.Order

/**
  * @author Joacim Zschimmer
  */
sealed trait TaskStepEnded {
  val outcome: Order.Outcome
}

final case class TaskStepSucceeded(
  variablesDiff: MapDiff[String, String],
  outcome: Order.Good)
extends TaskStepEnded

final case class TaskStepFailed(outcome: Order.Bad)
extends TaskStepEnded
