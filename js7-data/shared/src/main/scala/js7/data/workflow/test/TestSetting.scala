package js7.data.workflow.test

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.job.RelativePathExecutable
import js7.data.order.{Order, OrderId}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * For tests only.
  */
private[js7] object TestSetting
{
  val TestAgentPath = AgentPath("AGENT")
  val AJobName = WorkflowJob.Name("A")
  val BJobName = WorkflowJob.Name("B")
  val APathExecutable = RelativePathExecutable.checked("A.cmd", v1Compatible = true).orThrow
  val BPathExecutable = RelativePathExecutable.checked("B.cmd", v1Compatible = true).orThrow
  val AJob = WorkflowJob(TestAgentPath, APathExecutable, Map("JOB_A" -> StringConstant("A-VALUE")), parallelism = 3)
  val BJob = WorkflowJob(TestAgentPath, BPathExecutable, Map("JOB_B" -> StringConstant("B-VALUE")), parallelism = 3)
  val B1Job = WorkflowJob(TestAgentPath, BPathExecutable, Map("JOB_B1" -> StringConstant("B1-VALUE")), parallelism = 3)
  val AExecute = Execute(AJob)
  val BExecute = Execute(BJob)
  val TestPathExecutables = Vector(APathExecutable, BPathExecutable)

  val SimpleTestWorkflow = Workflow.of(
    WorkflowPath("WORKFLOW") ~ "VERSION",
    AExecute,
    BExecute)

  val TestOrder = Order(
    OrderId("TEST"),
    SimpleTestWorkflow.id /: Position(0),
    Order.Ready,
    Map("KEY" -> StringValue("VALUE")))
}
