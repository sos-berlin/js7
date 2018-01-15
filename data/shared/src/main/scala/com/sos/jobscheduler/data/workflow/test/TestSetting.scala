package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.Instruction.Job
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, WorkflowPath, Workflow}

/**
  * For tests only.
  */
private[jobscheduler] object TestSetting {

  val TestAgentPath = AgentPath("/AGENT")
  val AJobPath = JobPath("/A")
  val BJobPath = JobPath("/B")
  val TestJobPaths = Vector(AJobPath, BJobPath)

  val TestWorkflow = Workflow.Named(
    WorkflowPath("/WORKFLOW"),
    Workflow(Vector(
      Job(AgentJobPath(TestAgentPath, AJobPath)),
      Job(AgentJobPath(TestAgentPath, BJobPath)))))

  val TestOrder = Order(OrderId("TEST"), TestWorkflow.path, Order.Ready, payload = Payload(Map("VARIABLE" → "VALUE")))
}
