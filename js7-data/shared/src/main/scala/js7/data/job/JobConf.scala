package js7.data.job

import java.nio.charset.Charset
import js7.data.controller.ControllerId
import js7.data.job.JobConf.*
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.executable.WorkflowJob
import scala.concurrent.duration.FiniteDuration

final case class JobConf(
  jobKey: JobKey,
  workflowJob: WorkflowJob,
  workflow: Workflow,
  controllerId: ControllerId,
  sigkillDelay: FiniteDuration,
  systemEncoding: Charset):

  val jobResourcePaths: Seq[JobResourcePath] =
    jobResourcePathsFor(workflowJob, workflow)


object JobConf:
  def jobResourcePathsFor(workflowJob: WorkflowJob, workflow: Workflow): Seq[JobResourcePath] =
    (workflowJob.referencedJobResourcePaths ++ workflow.jobResourcePaths)
      .toVector.distinct
