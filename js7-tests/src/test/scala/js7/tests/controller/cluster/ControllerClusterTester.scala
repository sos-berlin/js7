package js7.tests.controller.cluster

import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.jobs.SleepJob
import js7.tests.testenv.ControllerClusterForScalaTest

private[cluster] trait ControllerClusterTester
extends OurTestSuite
with ControllerClusterForScalaTest
{
  protected def items = Seq(TestWorkflow)
}

object ControllerClusterTester
{
  private[cluster] val TestWorkflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      SleepJob.execute(AgentPath("AGENT"), parallelism = 2, arguments = Map(
        "sleep" -> expr("$sleep orElse 1")))))
}
