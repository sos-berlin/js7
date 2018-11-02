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
  val AExecute = Execute(WorkflowJob(TestAgentPath, ExecutablePath("/A.cmd"), Map("JOB_A" → "A-VALUE"),
    taskLimit = sys.runtime.availableProcessors))
  val BExecute = Execute(WorkflowJob(TestAgentPath, ExecutablePath("/B.cmd"), Map("JOB_B" → "B-VALUE"),
    taskLimit = sys.runtime.availableProcessors))
  val TestExecutablePaths = Vector(AExecute.job.executablePath, BExecute.job.executablePath)

  val SimpleTestWorkflow = Workflow.of(
    WorkflowPath("/WORKFLOW") % "VERSION",
    AExecute,
    BExecute)

  val TestOrder = Order(OrderId("TEST"), SimpleTestWorkflow.id, Order.Ready, payload = Payload(Map("VARIABLE" → "VALUE")))
}
