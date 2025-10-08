package js7.tests

import java.time.ZoneId
import js7.base.configutils.Configs.*
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.TestAlarmClock
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderAdded, OrderAwoke, OrderCaught, OrderFailed, OrderMoved, OrderOutcomeAdded, OrderRetrying, OrderStarted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.workflow.instructions.{Fail, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.Subagent
import js7.tests.RetryDelayTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.concurrent.TimeoutException

final class RetryDelayTest
extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = agentPath :: Nil
  protected val items = Nil

  private given ZoneId = ZoneId.of("Europe/Mariehamn")

  private val start = local("2022-10-25T12:00")
  private lazy val clock = TestAlarmClock(start)

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def subagentTestWiring = Subagent.TestWiring(
    clock = clock)

  "Retry with delay" in:
    val workflow = Workflow(
      WorkflowPath("RETRY-WITH-DELAY"),
      Seq(
        TryInstruction(
          Workflow.of(
            Fail()),
          Workflow.of(
            Retry()),
          retryDelays = Some(Vector(0.s, 1.s, 2.s)),
          maxTries = Some(5))))
    withItem(workflow) { workflow =>
      val orderId = OrderId("ORDER")
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow

      var eventId = eventWatch.await[OrderRetrying](
        o => o.key == orderId && o.event.delayedUntil.isDefined
      ).head.eventId

      clock += 999.ms
      intercept[TimeoutException](
        eventWatch.await[OrderAwoke](_.key == orderId, after = eventId, timeout = 200.ms))

      clock += 1.ms
      eventId = eventWatch.await[OrderAwoke](_.key == orderId).head.eventId
      eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).head.eventId

      clock += 1999.ms
      intercept[TimeoutException](
        eventWatch.await[OrderAwoke](_.key == orderId, after = eventId, timeout = 200.ms))

      clock += 1.ms
      eventId = eventWatch.await[OrderAwoke](_.key == orderId, after = eventId).head.eventId
      eventId = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId).head.eventId

      clock += 1999.ms
      intercept[TimeoutException](
        eventWatch.await[OrderAwoke](_.key == orderId, after = eventId, timeout = 200.ms))

      clock += 1.ms
      eventId = eventWatch.await[OrderAwoke](_.key == orderId, after = eventId).head.eventId
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.allStamped[OrderEvent]
        .map(o => o.timestamp -> o.value.event) ==
        Seq(
          start -> OrderAdded(workflow.id),
          start -> OrderMoved(Position(0) / "try+0" % 0),
          start -> OrderStarted,
          start -> OrderOutcomeAdded(OrderOutcome.failed),
          start -> OrderCaught(Position(0) / "catch+0" % 0),
          start -> OrderRetrying(),
          start -> OrderMoved(Position(0) / "try+1" % 0),

          start -> OrderOutcomeAdded(OrderOutcome.failed),
          start -> OrderCaught(Position(0) / "catch+1" % 0),
          start -> OrderRetrying(Some(start + 1.s)),

          start + 1.s -> OrderAwoke,
          start + 1.s -> OrderMoved(Position(0) / "try+2" % 0),
          start + 1.s -> OrderOutcomeAdded(OrderOutcome.failed),
          start + 1.s -> OrderCaught(Position(0) / "catch+2" % 0),
          start + 1.s -> OrderRetrying(Some(start + 3.s)),

          start + 3.s -> OrderAwoke,
          start + 3.s -> OrderMoved(Position(0) / "try+3" % 0),
          start + 3.s -> OrderOutcomeAdded(OrderOutcome.failed),
          start + 3.s -> OrderCaught(Position(0) / "catch+3" % 0),
          start + 3.s -> OrderRetrying(Some(start + 5.s)),

          start + 5.s -> OrderAwoke,
          start + 5.s -> OrderMoved(Position(0) / "try+4" % 0),
          start + 5.s -> OrderOutcomeAdded(OrderOutcome.failed),
          start + 5.s -> OrderFailed(Position(0) / "try+4" % 0)))
    }


object RetryDelayTest:
  private val agentPath = AgentPath("AGENT")
