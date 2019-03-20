package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}

/**
  * For tests only.
  */
private[jobscheduler] object TestSetting
{
  val TestAgentRefPath = AgentRefPath("/AGENT")
  val AJobName = WorkflowJob.Name("A")
  val BJobName = WorkflowJob.Name("B")
  val AExecutablePath = ExecutablePath("/A.cmd")
  val BExecutablePath = ExecutablePath("/B.cmd")
  val AJob = WorkflowJob(TestAgentRefPath, AExecutablePath, Map("JOB_A" -> "A-VALUE"), taskLimit = 3)
  val BJob = WorkflowJob(TestAgentRefPath, BExecutablePath, Map("JOB_B" -> "B-VALUE"), taskLimit = 3)
  val B1Job = WorkflowJob(TestAgentRefPath, BExecutablePath, Map("JOB_B1" -> "B1-VALUE"), taskLimit = 3)
  val AExecute = Execute(AJob)
  val BExecute = Execute(BJob)
  val TestExecutablePaths = Vector(AExecutablePath, BExecutablePath)

  val SimpleTestWorkflow = Workflow.of(
    WorkflowPath("/WORKFLOW") ~ "VERSION",
    AExecute,
    BExecute)

  val TestOrder = Order(OrderId("TEST"), SimpleTestWorkflow.id, Order.Ready, Map("KEY" -> "VALUE"))
}
