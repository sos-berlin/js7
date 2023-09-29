package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problems.DuplicateKey
import js7.base.test.OurTestSuite
import js7.base.time.{TestAlarmClock, Timestamp}
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderOrderAdded, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderPrompted, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{AddOrder, Fail, Prompt}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.AddOrderTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced

final class AddOrderTest extends OurTestSuite with ControllerAgentForScalaTest:
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  private val alarmClock = TestAlarmClock(Timestamp("2099-01-01T00:00:00Z"))

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(alarmClock))

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(aWorkflow, bWorkflow, c1Workflow, c2Workflow)

  "AddOrder" in:
    val orderId = OrderId("🔷")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderOrderAdded(
        OrderId("🟦"),
        bWorkflow.id,
        Map(
          "year" -> StringValue("2099")),
        deleteWhenTerminated = true),
      OrderMoved(Position(1)),
      OrderFinished()))
    eventWatch.await[OrderPrompted](_.key == OrderId("🟦"))

  "AddOrder with duplicate OrderId" in:
    val orderId = OrderId("🟧")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderOutcomeAdded(Outcome.Failed.fromProblem(DuplicateKey("OrderId", "🟦"))),
      OrderFailed(Position(0))))

  "AddOrder with access to clock" in:
    val orderId = OrderId("CLOCK")
    val events = controller.runOrder(FreshOrder(orderId, aWorkflow.path))
    assert(events.map(_.value) == Seq(
      OrderAdded(aWorkflow.id),
      OrderStarted,
      OrderOutcomeAdded(Outcome.Failed.fromProblem(DuplicateKey("OrderId", "🟦"))),
      OrderFailed(Position(0))))

  "AddOrder with startPosition and stopPositions (JS-2029)" in:
    locally:
      val orderId = OrderId("START-AND-STOP")
      val events = controller.runOrder(FreshOrder(orderId, c1Workflow.path)).map(_.value)
      assert(events == Seq(
        OrderAdded(c1Workflow.id),
        OrderStarted,
        OrderOrderAdded(
          OrderId("ADDED"),
          c2Workflow.id,
          startPosition = Some(Position(1)),
          stopPositions = Set(Position(2)),
          deleteWhenTerminated = true),
        OrderMoved(Position(1)),
        OrderFinished()))

    locally:
      val orderId = OrderId("ADDED")
      eventWatch.await[OrderDeleted](_.key == orderId)
      val events = eventWatch.eventsByKey[OrderEvent](orderId)
      assert(events == Seq(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(Some(subagentId), false),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderFinished(None),
        OrderDeleted))

object AddOrderTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val aWorkflow = Workflow(WorkflowPath("A-WORKFLOW") ~ "INITIAL",
    Seq(
      AddOrder(
        orderId = expr("'🟦'"),
        bWorkflow.path,
        Map(
          "year" -> expr("now('yyyy')")),
        deleteWhenTerminated = true)))

  private lazy val bWorkflow = Workflow(WorkflowPath("B-WORKFLOW") ~ "INITIAL",
    Seq(
      Prompt(expr("'?'"))))

  private val c1Workflow = Workflow(WorkflowPath("C1-WORKFLOW") ~ "INITIAL",
    Seq(
      AddOrder(
        orderId = expr("'ADDED'"),
        c2Workflow.path,
        startPosition = Some(Position(1)),
        stopPositions = Set(Position(2)),
        deleteWhenTerminated = true)))

  private lazy val c2Workflow = Workflow(WorkflowPath("C2-WORKFLOW") ~ "INITIAL",
    Seq(
      Prompt(expr("'?'")),
      EmptyJob.execute(agentPath),
      Fail(None)))
