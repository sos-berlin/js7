package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}

/**
  * For tests only.
  */
private[jobscheduler] object TestSetting {

  val TestAgentPath = AgentPath("/AGENT")
  val AJobName = WorkflowJob.Name("A")
  val BJobName = WorkflowJob.Name("B")
  val AJob = WorkflowJob(TestAgentPath, ExecutablePath("/A.cmd"), Map("JOB_A" → "A-VALUE"), taskLimit = 3)
  val BJob = WorkflowJob(TestAgentPath, ExecutablePath("/B.cmd"), Map("JOB_B" → "B-VALUE"), taskLimit = 3)
  val B1Job = WorkflowJob(TestAgentPath, ExecutablePath("/B.cmd"), Map("JOB_B1" → "B1-VALUE"), taskLimit = 3)
  val AExecute = Execute(AJob)
  val BExecute = Execute(BJob)
  val TestExecutablePaths = Vector(AExecute.job.executablePath, BExecute.job.executablePath)

  val SimpleTestWorkflow = Workflow.of(
    WorkflowPath("/WORKFLOW") % "VERSION",
    AExecute,
    BExecute)

  val TestOrder = Order(OrderId("TEST"), SimpleTestWorkflow.id, Order.Ready, payload = Payload(Map("VARIABLE" → "VALUE")))
}
