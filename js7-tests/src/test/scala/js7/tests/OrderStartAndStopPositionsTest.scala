package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, If}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.OrderStartAndStopPositionsTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AnyFreeSpec

final class OrderStartAndStopPositionsTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  "Invalid position" in {
    def addOrder(startPosition: Option[Position] = None, stopPosition: Option[Position] = None) =
      controllerApi
        .addOrder(FreshOrder(
          OrderId("INVALID"), workflow.path,
          startPosition = startPosition,
          stopPosition = stopPosition))
        .await(99.s)

    val unknownPosition = Position(workflow.instructions.length)
    val nestedPosition = Position(0) / BranchId.Then % 0
    assert(
      addOrder(startPosition = Some(unknownPosition)) == Left(Problem(
        "Unknown position 5 in Workflow:A-WORKFLOW~INITIAL")))
    assert(
      addOrder(startPosition = Some(nestedPosition)) == Left(Problem(
        "Order's startPosition or stopPosition must not be in a Workflow branch")))
    assert(
      addOrder(stopPosition = Some(unknownPosition)) == Left(Problem(
        "Unknown position 5 in Workflow:A-WORKFLOW~INITIAL")))
    assert(
      addOrder(stopPosition = Some(nestedPosition)) == Left(Problem(
        "Order's startPosition or stopPosition must not be in a Workflow branch")))
  }

  "startPosition == stopPosition" in {
    val orderId = OrderId("A")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, workflow.path,
      startPosition = Some(Position(1)),
      stopPosition = Some(Position(1))))
    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id,
        startPosition = Some(Position(1)),
        stopPosition = Some(Position(1))),
      OrderStarted,
      OrderFinished))
  }

  "startPosition and stopPosition" in {
    val orderId = OrderId("B")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, workflow.path,
      startPosition = Some(Position(1)),
      stopPosition = Some(Position(2))))
    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id,
        startPosition = Some(Position(1)),
        stopPosition = Some(Position(2))),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2)),

      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "stopPosition points to an If statement" in {
    val orderId = OrderId("C")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, workflow.path,
      startPosition = Some(Position(1)),
      stopPosition = Some(Position(3))))
    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id,
        startPosition = Some(Position(1)),
        stopPosition = Some(Position(3))),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2)),

      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(3)),

      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }
}

object OrderStartAndStopPositionsTest
{
  private val agentPath = AgentPath("A-AGENT")

  private val workflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL", Seq(
    If(expr("true"), Workflow.of(Fail())),
    EmptyJob.execute(agentPath),
    EmptyJob.execute(agentPath),
    If(expr("true"), Workflow.of(Fail()))))
}
