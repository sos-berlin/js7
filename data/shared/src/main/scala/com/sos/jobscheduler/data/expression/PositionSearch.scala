package com.sos.jobscheduler.data.expression

import com.sos.jobscheduler.data.workflow.Label
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob

sealed trait PositionSearch

object PositionSearch
{
  final case class ByWorkflowJob(jobName: WorkflowJob.Name)
  extends PositionSearch
  {
    override def toString = s"ByWorkflowJob(${jobName.string})"
  }

  final case class ByLabel(label: Label)
  extends PositionSearch
  {
    override def toString = s"ByLabel(${label.string})"
  }
}
