package js7.tests.internaljob

import js7.base.configutils.Configs.*
import js7.data.agent.AgentPath
import js7.data.job.{InternalExecutable, JobResource, JobResourcePath}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.FastparseExpressionParser.expr
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.internaljob.SimpleBlockingInternalJobTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalatest.freespec.AnyFreeSpec

final class SimpleBlockingInternalJobTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(workflow, jobResource)
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  "Step.env" in {
    val outcome = controller
      .runOrder(FreshOrder(OrderId("TestEnvBlockingInternalJob"), workflow.path))
      .map(_.value)
      .collectFirst { case OrderProcessed(outcome) => outcome }
    assert(outcome contains Outcome.succeeded)
  }
}

object SimpleBlockingInternalJobTest
{
  private val agentPath = AgentPath("AGENT")
  private val jobResource = JobResource(
    JobResourcePath("JOB-RESOURCE"),
    env = Map("ENV" -> expr("'ENV-VALUE'")))

  private val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "INITIAL",
    Execute(
      WorkflowJob(
        agentPath,
        InternalExecutable(
          classOf[TestEnvBlockingInternalJob].getName),
        jobResourcePaths = Seq(jobResource.path))))
}
