package js7.data.job

import js7.data.controller.ControllerId
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.concurrent.duration.FiniteDuration

final case class JobConf(
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  workflow: Workflow,
  controllerId: ControllerId,
  sigkillDelay: FiniteDuration)
{
  val jobResourcePaths: Seq[JobResourcePath] =
    (workflowJob.referencedJobResourcePaths ++ workflow.jobResourcePaths)
      .toVector.distinct
}
