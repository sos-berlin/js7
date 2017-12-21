package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowOverview(
  path: WorkflowPath)

object WorkflowOverview {
  def fromWorkflow(workflow: WorkflowGraph.Named) = WorkflowOverview(path = workflow.path)

  implicit val jsonCodec = deriveCirceCodec[WorkflowOverview]
}
