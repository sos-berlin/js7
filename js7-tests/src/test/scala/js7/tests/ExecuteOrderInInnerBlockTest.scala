package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.value.BooleanValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fork, If, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, BranchPath, Position, PositionOrLabel}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ExecuteOrderInInnerBlockTest.*
import js7.tests.jobs.{EmptyJob, FailingJob}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}

final class ExecuteOrderInInnerBlockTest
extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

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

  "Invalid innerBlock, or startPosition or stopPositions not in innerBlock" in:
    def addOrder(
      innerBlock: BranchPath,
      startPosition: Option[Position] = None,
      stopPositions: Set[PositionOrLabel] = Set.empty)
    : Checked[Boolean] =
      controller.api
        .addOrder(FreshOrder(
          OrderId("INVALID"), workflow.path,
          innerBlock = innerBlock,
          startPosition = startPosition,
          stopPositions = stopPositions))
        .await(99.s)

    assert:
      addOrder(Position(0) / "then") == Left(Problem(
        "Instruction 'Execute.Anonymous' does not have a nested workflow for branch 'then'"))
    assert:
      addOrder(forkBranchPath, startPosition = Some(Position(1))) == Left(Problem:
        "Position 1 must be in innerBlock=1/then:0/fork+BRANCH")
    assert:
      addOrder(forkBranchPath, stopPositions = Set(Position(2), forkBranchPath % 1)) == Left:
        Problem("Position 2 must be in innerBlock=1/then:0/fork+BRANCH")

  "Execute in a Fork block" in:
    val innerBlock = forkBranchPath

    val stampedEvents = controller.runOrder:
      FreshOrder(OrderId("FORK"), workflow.path, innerBlock = innerBlock)

    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id, innerBlock = innerBlock),

      OrderMoved(innerBlock % 0 / "then" % 1 / "try+0" % 1),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(innerBlock % 1),

      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "Execute in a 'then' block of a Fork block" in:
    val innerBlock = forkBranchPath % 0 / "then" % 0 / "then"

    val stampedEventsevents = controller.runOrder:
      FreshOrder(OrderId("FORK-THEN"), workflow.path,
        innerBlock = innerBlock, startPosition = Some(innerBlock % 0))

    assert(stampedEventsevents.map(_.value) == Seq(
      OrderAdded(workflow.id, innerBlock = innerBlock, startPosition = Some(innerBlock % 0)),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(innerBlock % 1),

      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "Leave innerBlock on failure, ignoring outer catch block" in:
    val innerBlock = forkBranchPath % 0 / "then" % 1 / "try" % 0 / "then"

    val stampedEvents = controller.runOrder:
      FreshOrder(OrderId("FORK-FAIL"), workflow.path, innerBlock = innerBlock,
        arguments = Map("FAIL" -> BooleanValue.True))

    assert(stampedEvents.map(_.value) == Seq(
      OrderAdded(workflow.id, innerBlock = innerBlock,
        arguments = Map("FAIL" -> BooleanValue.True)),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(toLocalSubagentId(agentPath)),
      OrderProcessed(FailingJob.outcome),

      OrderDetachable,
      OrderDetached,
      OrderFailed(innerBlock % 0)))

  "JS-2093 OutOfMemoryError" in:
    val workflow = Workflow(WorkflowPath("OOM"), Seq:
      Fork.forTest(Seq:
        "BRANCH" -> Workflow.of:
          EmptyJob.execute(agentPath)))

    withItem(workflow): workflow =>
      val innerBlock = Position(0) / BranchId.fork("BRANCH")
      controller.runOrder(
        FreshOrder(OrderId("OOM"), workflow.path,
          innerBlock = innerBlock,
          startPosition = Some(innerBlock % 0),
          stopPositions = Set(innerBlock % 1),
          arguments = Map("FAIL" -> BooleanValue.True)))


object ExecuteOrderInInnerBlockTest:
  private val agentPath = AgentPath("A-AGENT")

  private val workflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    If(expr("false"), Workflow.of(
      Fork.forTest(Seq("BRANCH" -> Workflow.of(
        If(expr("true"), Workflow.of(
        If(expr("false"), Workflow.of(
          EmptyJob.execute(agentPath))),
          TryInstruction(
            Workflow.of(
              If(expr("$FAIL ? false"), Workflow.of(
          FailingJob.execute(agentPath))),
              EmptyJob.execute(agentPath)),
            catchWorkflow = Workflow.empty)))))))),
    EmptyJob.execute(agentPath)))

  private val forkBranchPath = Position(1) / "then" % 0 / BranchId.fork("BRANCH")
