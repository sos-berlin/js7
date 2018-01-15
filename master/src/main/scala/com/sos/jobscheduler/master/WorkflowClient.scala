package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.workflow.{WorkflowPath, Workflow, WorkflowsOverview}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait WorkflowClient {
  protected implicit def executionContext: ExecutionContext

  def namedWorkflow(workflowPath: WorkflowPath): Future[Option[Workflow.Named]]

  def namedWorkflows: Future[Stamped[Seq[Workflow.Named]]]

  def workflowOverviews: Future[Stamped[Seq[WorkflowPath]]] =
    for (o ← namedWorkflows) yield
      o map (_ map (_.path))

  def workflowsOverview: Future[WorkflowsOverview] =
    for (c ← workflowCount) yield
      WorkflowsOverview(workflowCount = c)

  def workflowCount: Future[Int]
}
