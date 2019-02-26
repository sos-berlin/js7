package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{Order, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}

/**
  * For tests only.
  */
private[jobscheduler] object TestSetting {

  val TestAgentRefPath = AgentRefPath("/AGENT")
  val AJobName = WorkflowJob.Name("A")
  val BJobName = WorkflowJob.Name("B")
  val AJob = WorkflowJob(TestAgentRefPath, ExecutablePath("/A.cmd"), Map("JOB_A" -> "A-VALUE"), taskLimit = 3)
  val BJob = WorkflowJob(TestAgentRefPath, ExecutablePath("/B.cmd"), Map("JOB_B" -> "B-VALUE"), taskLimit = 3)
  val B1Job = WorkflowJob(TestAgentRefPath, ExecutablePath("/B.cmd"), Map("JOB_B1" -> "B1-VALUE"), taskLimit = 3)
  val AExecute = Execute(AJob)
  val BExecute = Execute(BJob)
  val TestExecutablePaths = Vector(AExecute.job.executablePath, BExecute.job.executablePath)

  val SimpleTestWorkflow = Workflow.of(
    WorkflowPath("/WORKFLOW") % "VERSION",
    AExecute,
    BExecute)

  val TestOrder = Order(OrderId("TEST"), SimpleTestWorkflow.id, Order.Ready, payload = Payload(Map("VARIABLE" -> "VALUE")))
}
