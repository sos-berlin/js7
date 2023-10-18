package js7.tests.cluster.controller

import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.item.InventoryItem
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.jobs.SleepJob
import js7.tests.testenv.ControllerClusterForScalaTest

private[cluster] trait ControllerClusterTester
extends OurTestSuite, ControllerClusterForScalaTest:
  
  protected def items: Seq[InventoryItem] =
    Seq(TestWorkflow)


object ControllerClusterTester:
  private[cluster] val TestWorkflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      SleepJob.execute(AgentPath("AGENT"), parallelism = 2, arguments = Map(
        "sleep" -> expr("$sleep ? 1")))))
