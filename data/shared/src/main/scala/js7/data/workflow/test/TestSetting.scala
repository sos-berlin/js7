package js7.data.workflow.test

import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * For tests only.
  */
private[js7] object TestSetting
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
