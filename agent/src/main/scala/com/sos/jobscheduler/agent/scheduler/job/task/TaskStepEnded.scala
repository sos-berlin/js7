package com.sos.jobscheduler.agent.scheduler.job.task

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.job.ReturnCode

/**
  * @author Joacim Zschimmer
  */
sealed trait TaskStepEnded

final case class TaskStepSucceeded(
  variablesDiff: MapDiff[String, String],
  returnCode: ReturnCode)
extends TaskStepEnded

final case class TaskStepFailed(problem: Problem)
extends TaskStepEnded
