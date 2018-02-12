package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, IfReturnCode, Job}
import com.sos.jobscheduler.data.workflow.{JobPath, Workflow, WorkflowPath}

/**
  * For tests only.
  */
private[jobscheduler] object TestSetting {

  val TestAgentPath = AgentPath("/AGENT")
  val AJob = Job(JobPath("/A"), TestAgentPath)
  val BJob = Job(JobPath("/B"), TestAgentPath)
  val TestJobPaths = Vector(AJob.jobPath, BJob.jobPath)

  val SimpleTestWorkflow = Workflow.Named(
    WorkflowPath("/WORKFLOW"),
    Workflow.of(
      AJob,
      BJob))

  val ComplexTestWorkflow = Workflow.of(
    AJob,
    IfReturnCode(
      ReturnCode(1) :: Nil,
      thenWorkflow = Workflow.of(AJob),
      elseWorkflow = Some(Workflow.of(BJob))),
    ForkJoin.of(
      "🥕" → Workflow.of(AJob, AJob),
      "🍋" → Workflow.of(BJob, BJob)),
    BJob)

  val TestOrder = Order(OrderId("TEST"), SimpleTestWorkflow.path, Order.Ready, payload = Payload(Map("VARIABLE" → "VALUE")))
}
