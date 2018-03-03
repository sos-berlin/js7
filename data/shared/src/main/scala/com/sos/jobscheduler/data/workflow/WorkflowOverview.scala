package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowOverview(
  path: WorkflowPath)

object WorkflowOverview {
  def fromWorkflow(workflow: Workflow) = WorkflowOverview(path = workflow.path)

  implicit val jsonCodec = deriveCodec[WorkflowOverview]
}
