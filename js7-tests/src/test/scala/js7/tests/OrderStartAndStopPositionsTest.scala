package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCaught, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, Fork, If, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Label, Position, PositionOrLabel}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.OrderStartAndStopPositionsTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class OrderStartAndStopPositionsTest extends OurTestSuite, ControllerAgentForScalaTest:
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow, tryWorkflow)

  "Invalid position" in:
    def addOrder(
      startPosition: Option[Position] = None,
      stopPositions: Set[PositionOrLabel] = Set.empty)
    =
      controller.api
        .addOrder(FreshOrder(
          OrderId("INVALID"), workflow.path,
          startPosition = startPosition,
          stopPositions = stopPositions))
        .await(99.s)

    val unknownPosition = Position(workflow.instructions.length)
    val forkedPosition = Position(0) / "then" % 0 / BranchId.fork("BRANCH") % 0
    assert(
      addOrder(startPosition = Some(unknownPosition)) == Left(Problem(
        "Unknown position 5 in Workflow:A-WORKFLOW~INITIAL")))
    assert(
      addOrder(startPosition = Some(forkedPosition)) == Left(Problem(
        "Order's startPosition or one of its stopPositions is not reachable: 0/then:0/fork+BRANCH:0")))
    assert(
      addOrder(stopPositions = Set(unknownPosition)) == Left(Problem(
        "Unknown position 5 in Workflow:A-WORKFLOW~INITIAL")))
    assert(
      addOrder(stopPositions = Set(forkedPosition)) == Left(Problem(
        "Order's startPosition or one of its stopPositions is not reachable: 0/then:0/fork+BRANCH:0")))

  "startPosition == stopPositions" in:
    val orderId = OrderId("A")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, workflow.path,
      startPosition = Some(Position(1)),
      stopPositions = Set(Position(1))))

    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id,
        startPosition = Some(Position(1)),
        stopPositions = Set(Position(1))),
      OrderStarted,
      OrderFinished()))

  "startPosition and stopPositions" in:
    val orderId = OrderId("B")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, workflow.path,
      startPosition = Some(Position(1)),
      stopPositions = Set(Position(2))))

    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id,
        startPosition = Some(Position(1)),
        stopPositions = Set(Position(2))),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(2)),

      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "stopPositions point to an If statement" in:
    val orderId = OrderId("C")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, workflow.path,
      startPosition = Some(Position(1)),
      stopPositions = Set(Position(3))))
    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id,
        startPosition = Some(Position(1)),
        stopPositions = Set(Position(3))),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(2)),

      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(3)),

      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "startPosition and stopPosition point into If statements" in:
    val orderId = OrderId("D")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, tryWorkflow.path,
      startPosition = Some(Position(0) / "then" % 0),
      stopPositions = Set(Label("LABEL"))))

    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(tryWorkflow.id,
        startPosition = Some(Position(0) / "then" % 0),
        stopPositions = Set(Label("LABEL"))),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1) / "try+0" % 0),

      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1) / "try+0" % 1),

      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1) / "try+0" % 2 / "then" % 0),
      // LABEL reached

      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "startPosition points into a Try statement" in:
    val orderId = OrderId("E")
    val stampedEvents = controller.runOrder(FreshOrder(
      orderId, tryWorkflow.path,
      startPosition = Some(Label("LABEL")),
      stopPositions = Set(Position(1) / "catch" % 0)))

    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(tryWorkflow.id,
        startPosition = Some(Position(1) / "try" % 2 / "then" % 0),
        stopPositions = Set(Position(1) / "catch" % 0)),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1) / "try" % 3 / "then" % 0),

      OrderOutcomeAdded(OrderOutcome.failed),
      OrderCaught(Position(1) / "catch+0" % 0),

      OrderDetachable,
      OrderDetached,
      OrderFinished()))


object OrderStartAndStopPositionsTest:
  private val agentPath = AgentPath("A-AGENT")

  private val workflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL", Seq(
    If(expr("true")):
      Fork.of("BRANCH" -> Workflow.of(Fail())),
    EmptyJob.execute(agentPath),
    EmptyJob.execute(agentPath),
    If(expr("true")):
      Fail()))

  private val tryWorkflow = Workflow(WorkflowPath("TRY-WORKFLOW") ~ "INITIAL", Seq(
    If(expr("true")):
      EmptyJob.execute(agentPath),
    TryInstruction(
      Workflow.of(
        EmptyJob.execute(agentPath),
        EmptyJob.execute(agentPath),
        If(expr("true")):
          "LABEL" @: EmptyJob.execute(agentPath),
        If(expr("true")):
          Fail()),
      Workflow.of(
        "CATCHED" @: Fail()))))
