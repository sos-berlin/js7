package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.job.{RelativePathExecutable, ShellScriptExecutable}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.NumberValue
import js7.data.value.expression.Expression.NumericConstant
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.JobTimeoutTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.sleepingScript
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now

final class JobTimeoutTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  override def beforeAll() = {
    directoryProvider.agents.head.writeExecutable(RelativePathExecutable("test.cmd"), (isWindows ?? "@echo off\n") + "exit 3")
    super.beforeAll()
  }

  "timeout" in {
    // Warm-up
    controller.runOrder(FreshOrder(OrderId("WARM-UP"), workflowPath = workflow.path), delete = true)
      .map(_.value)

    val t = now
    val events = controller.runOrder(FreshOrder(OrderId("ORDER"), workflowPath = workflow.path))
      .map(_.value)
    logger.info(t.elapsed.pretty)
    assert(t.elapsed >= timeout)
    assert(events ==
      Vector(
        OrderAdded(workflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.TimedOut(Outcome.Failed(Map(
          "returnCode" -> NumberValue(128 + SIGTERM.number))))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))))
  }
}

object JobTimeoutTest
{
  private val agentPath = AgentPath("AGENT")
  private val timeout = 200.ms
  private val logger = Logger[this.type]

  private def workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL", Seq(
    Execute(WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        sleepingScript("SLEEP"), Map(
          "SLEEP" -> NumericConstant(9))),
      timeout = Some(timeout)))))
}
