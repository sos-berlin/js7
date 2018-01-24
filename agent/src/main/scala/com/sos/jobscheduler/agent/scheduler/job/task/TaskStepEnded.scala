package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.order.Outcome

/**
  * @author Joacim Zschimmer
  */
sealed trait TaskStepEnded {
  val outcome: Outcome
}

final case class TaskStepSucceeded(
  variablesDiff: MapDiff[String, String],
  outcome: Outcome.Succeeded)
extends TaskStepEnded

final case class TaskStepFailed(outcome: Outcome.Disrupted)
extends TaskStepEnded
