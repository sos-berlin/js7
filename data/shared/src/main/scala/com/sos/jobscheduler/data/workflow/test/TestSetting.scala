package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.{JobPath, Workflow, WorkflowPath}

/**
  * For tests only.
  */
private[jobscheduler] object TestSetting {

  val TestAgentPath = AgentPath("/AGENT")
  val AJob = Job(JobPath("/A"), TestAgentPath)
  val BJob = Job(JobPath("/B"), TestAgentPath)
  val TestJobPaths = Vector(AJob.jobPath, BJob.jobPath)

  val SimpleTestWorkflow = Workflow.of(
    WorkflowPath("/WORKFLOW"),
    AJob,
    BJob)

  val TestOrder = Order(OrderId("TEST"), SimpleTestWorkflow.path, Order.Ready, payload = Payload(Map("VARIABLE" → "VALUE")))
}
