package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPath}

@javaApi
final case class JWorkflowId(underlying: WorkflowId)
extends JFileBasedId[WorkflowPath]
{
  protected type ScalaPath = WorkflowPath
  protected type Underlying = WorkflowId

  def path = underlying.path
}

@javaApi
object JWorkflowId
{
  def of(path: String, commitId: String): JWorkflowId =
    JWorkflowId(WorkflowPath(path) ~ commitId)
}
