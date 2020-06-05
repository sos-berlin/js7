package js7.agent.scheduler.job.task

import js7.base.problem.Problem
import js7.data.job.ReturnCode

/**
  * @author Joacim Zschimmer
  */
sealed trait TaskStepEnded

final case class TaskStepSucceeded(
  keyValues: Map[String, String],
  returnCode: ReturnCode)
extends TaskStepEnded

final case class TaskStepFailed(problem: Problem)
extends TaskStepEnded
