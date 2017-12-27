package com.sos.jobscheduler.master

import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.workflow.{WorkflowGraph, WorkflowPath, WorkflowScript, WorkflowsOverview}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait WorkflowClient {
  protected implicit def executionContext: ExecutionContext

  def workflow(workflowPath: WorkflowPath): Future[Option[WorkflowGraph.Named]]

  def workflows: Future[Stamped[Seq[WorkflowGraph.Named]]]

  final def workflowScripts: Future[Stamped[Seq[WorkflowScript.Named]]] =
    for (stamped ← workflows) yield
      stamped.copy(value = stamped.value flatMap (_.toWorkflowScriptNamed))

  def workflowOverviews: Future[Stamped[Seq[WorkflowPath]]] =
    for (o ← workflows) yield
      o map (_ map (_.path))

  def workflowsOverview: Future[WorkflowsOverview] =
    for (c ← workflowCount) yield
      WorkflowsOverview(workflowCount = c)

  def workflowCount: Future[Int]
}
