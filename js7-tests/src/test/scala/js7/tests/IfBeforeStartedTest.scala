package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestAlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderAdded, OrderDeleted, OrderFinished, OrderMoved, OrderSaid, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.{If, Say}
import js7.data.workflow.position.BranchPath.syntax.%
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.IfBeforeStartedTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions

final class IfBeforeStartedTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  private lazy val clock = TestAlarmClock(ts"2100-01-01T12:00:00Z")

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  protected def agentPaths = agentPath :: Nil
  protected def items = Nil

  "If is executed BEFORE OrderStarted, not querying the clock (anticipating)" in:
    // Because 'if' is executed ancipating, ahead of scheduled time,
    // the Engine may attach the Order to an Agent ahead of scheduled time.
    clock.resetTo(ts"2100-01-01T12:00:00Z")

    withItem(
      Workflow.of:
        If(expr"""$$a""").Then:
          Say(expr"'THEN'")
        .Else:
          Say(expr"'ELSE'")
    ): workflow =>
      val orderId = nextOrderId()
      addOrder:
        FreshOrder(orderId, workflow.path,
          scheduledFor = Some(ts"2100-01-01T13:00:00Z"),
          arguments = Map("a" -> true))
      controller.awaitNextKey[OrderAdded](orderId)
      assert(controllerState.idToOrder(orderId).isState[Order.Fresh])

      clock := ts"2100-01-01T13:00:00Z"
      controller.awaitNextKey[OrderFinished](orderId)

      assert:
        controller.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id,
            scheduledFor = Some(ts"2100-01-01T13:00:00Z"),
            arguments = Map("a" -> true),
            deleteWhenTerminated = true),
          OrderMoved(Position(0) / "then" % 0),
          OrderStarted,
          OrderSaid("THEN"),
          OrderMoved(Position(1)),
          OrderFinished(),
          OrderDeleted)

  // When an impure function (now, jobResource) is caller, the if-instruction must wait until
  // the Order's scheduled time has been reached.
  /// An impure function can only be called at the Controller, ///
  /// because only the Execute and the Fork-instruction       ///
  /// may attach the Order, but then, the Order is started.   ///

  "If, when querying the clock, is executed AFTER OrderStarted (not anticipating)" in :
    clock.resetTo(ts"2100-01-01T12:00:00Z")

    withItem(
      Workflow.of:
        If(expr"""now(format='HH:mm', timezone=workflow.timezone) == '13:00'""").Then:
          Say(expr"'THEN'")
        .Else:
          Say(expr"'ELSE'")
    ): workflow =>
      val orderId = nextOrderId()
      addOrder:
        FreshOrder(orderId, workflow.path, scheduledFor = Some(ts"2100-01-01T13:00:00Z"))
      controller.awaitNextKey[OrderAdded](orderId)
      assert(controllerState.idToOrder(orderId).isState[Order.Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))

      clock := ts"2100-01-01T13:00:00Z"
      controller.awaitNextKey[OrderTerminated](orderId)

      assert:
        controller.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id,
            scheduledFor = Some(ts"2100-01-01T13:00:00Z"),
            deleteWhenTerminated = true),
          OrderStarted,
          OrderMoved(Position(0) / "then" % 0),
          OrderSaid("THEN"),
          OrderMoved(Position(1)),
          OrderFinished(),
          OrderDeleted)

  "If, when querying a JobResource, is executed AFTER OrderStarted (not anticipating)" in :
    // This can happen only at the Controller, because only the Execute and the Fork-instruction
    // may attach the Order, but these instructions start the Order.

    clock.resetTo(ts"2100-01-01T12:00:00Z")

    val jobResourcePath = JobResourcePath("JOB-RESOURCE")
    withItems((
      Workflow(
        WorkflowPath("WORKFLOW"),
        Seq:
          If(expr"jobResourceVariable('JOB-RESOURCE', 'hhmm') == '13:00'").Then:
            Say(expr"'THEN'")
          .Else:
            Say(expr"'ELSE'"),
        jobResourcePaths = Seq(jobResourcePath)),
      JobResource(jobResourcePath,
        variables = Map(
          "hhmm" -> expr"now(format='HH:mm', timezone=workflow.timezone)"))
    )): (workflow, jobResource) =>
      val orderId = nextOrderId()
      addOrder:
        FreshOrder(orderId, workflow.path,
          scheduledFor = Some(ts"2100-01-01T13:00:00Z"))
      controller.awaitNextKey[OrderAdded](orderId)
      assert(controllerState.idToOrder(orderId).isState[Order.Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))

      clock := ts"2100-01-01T13:00:00Z"
      controller.awaitNextKey[OrderTerminated](orderId)

      assert:
        controller.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id,
            scheduledFor = Some(ts"2100-01-01T13:00:00Z"),
            deleteWhenTerminated = true),
          OrderStarted,
          OrderMoved(Position(0) / "then" % 0),
          OrderSaid("THEN"),
          OrderMoved(Position(1)),
          OrderFinished(),
          OrderDeleted)


object IfBeforeStartedTest:
  private val agentPath = AgentPath("AGENT")
