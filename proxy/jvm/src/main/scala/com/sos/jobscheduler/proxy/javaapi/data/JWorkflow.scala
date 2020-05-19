package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import io.vavr.control.{Either => VEither}

@javaApi
final case class JWorkflow(underlying: Workflow)
extends JFileBased[JWorkflow, WorkflowPath]
{
  protected type Underlying = Workflow

  def companion = JWorkflow

  def id = JWorkflowId(underlying.id)
}

@javaApi
object JWorkflow extends JJsonable.Companion[JWorkflow]
{
  override def fromJson(jsonString: String): VEither[Problem, JWorkflow] =
    super.fromJson(jsonString)

  def jsonEncoder = Workflow.jsonEncoder
  def jsonDecoder = Workflow.jsonDecoder
}
