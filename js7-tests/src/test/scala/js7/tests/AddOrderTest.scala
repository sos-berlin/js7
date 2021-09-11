package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problems.DuplicateKey
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderFailed, OrderFinished, OrderMoved, OrderOrderAdded, OrderPrompted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{AddOrder, Prompt}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.AddOrderTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AddOrderTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(aWorkflow, bWorkflow)

  "AddOrder" in {
    val orderId = OrderId("🔵")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderOrderAdded(OrderId("🟦"), bWorkflow.id, deleteWhenTerminated = true),
      OrderMoved(Position(1)),
      OrderFinished))
    eventWatch.await[OrderPrompted](_.key == OrderId("🟦"))
  }

  "AddOrder with duplicate OrderId" in {
    val orderId = OrderId("🟠")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderFailed(
        Position(0),
        Some(Outcome.Failed.fromProblem(
          DuplicateKey("OrderId", "🟦"))))))
  }
}

object AddOrderTest
{
  private val agentPath = AgentPath("AGENT")

  private lazy val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL",
    Seq(
      AddOrder(expr("'🟦'"), bWorkflow.path, deleteWhenTerminated = true)))

  private val bWorkflow = Workflow(WorkflowPath("B-WORKFLOW") ~ "INITIAL",
    Seq(
      Prompt(expr("'?'"))))
}
