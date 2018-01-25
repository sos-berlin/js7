package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.Outcome

/**
  * @author Joacim Zschimmer
  */
sealed trait TaskStepEnded

final case class TaskStepSucceeded(
  variablesDiff: MapDiff[String, String],
  returnCode: ReturnCode)
extends TaskStepEnded

final case class TaskStepFailed(disrupted: Outcome.Disrupted)
extends TaskStepEnded
