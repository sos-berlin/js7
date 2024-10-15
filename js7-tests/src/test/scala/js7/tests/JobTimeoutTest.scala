package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, HistoricOutcome, OrderId, OrderOutcome}
import js7.data.value.NumberValue
import js7.data.value.expression.Expression
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fail, If, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.JobTimeoutTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import scala.concurrent.duration.Deadline.now

final class JobTimeoutTest extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "Timeout" in:
    val workflow = Workflow(WorkflowPath("TIMEOUT"), Seq(
      TryInstruction(
        Workflow.of:
          Execute:
            WorkflowJob(
              agentPath,
              ShellScriptExecutable:
                if isWindows then
                  s"""@echo off
                     |ping -n ${jobDuration.toSeconds + 1} 127.0.0.1 >nul
                     |""".stripMargin
                else
                  s"""#!/usr/bin/env bash
                     |set -euo pipefail
                     |i=${10 * jobDuration.toSeconds}
                     |while [ $$i -ge 0 ]; do sleep 0.1; done
                     |""".stripMargin,
                timeout = Some(timeout)),
        Workflow.of:
          If(expr("!timedOut"),
            Workflow.of:
              Fail(Some(expr("'💥 NOT TIMED OUT'")))))))

    withItem(workflow): workflow =>
      // Warm-up
      controller.runOrder(
        FreshOrder(OrderId("WARM-UP"), workflowPath = workflow.path, deleteWhenTerminated = true))

      val t = now
      val events = controller.runOrder:
        FreshOrder(OrderId("TIMEOIUT"), workflowPath = workflow.path, deleteWhenTerminated = true)
      .map(_.value)
      val elapsed = t.elapsed
      logger.info(elapsed.pretty)
      assert(elapsed >= timeout && elapsed < jobDuration / 2)
      assert(events ==
        Vector(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.TimedOut(OrderOutcome.Failed(Map(
            "returnCode" -> NumberValue(sigtermReturnCode))))),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderMoved(Position(1)),
          OrderDetachable,
          OrderDetached,
          OrderFinished(),
          OrderDeleted))

  "No timeout" in:
    val workflow = Workflow(WorkflowPath("NO-TIMEOUT"), Seq(
      TryInstruction(
        Workflow.of:
          Fail(),
        Workflow.of:
          If(expr("timedOut()"),
            Workflow.of:
              Fail(Some(expr("'💥 TIMED OUT'")))))))

    withItem(workflow): workflow =>
      val events = controller.runOrder:
        FreshOrder(OrderId("NO-TIMEOUT"), workflowPath = workflow.path, deleteWhenTerminated = true)
      .map(_.value)
      assert(events ==
        Vector(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderStarted,
          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderMoved(Position(1)),
          OrderFinished(),
          OrderDeleted))

  "returnCode and named values after caught timed-out script execution" in :
    val workflow = Workflow(WorkflowPath("WORKFLOW"), Seq(
      TryInstruction(
        Workflow.of:
          Execute:
            WorkflowJob(
              agentPath,
              ShellScriptExecutable:
                if isWindows then
                  s"""@echo off
                     |ping -n ${jobDuration.toSeconds + 1} 127.0.0.1 >nul
                     |""".stripMargin
                else
                  s"""#!/usr/bin/env bash
                     |set -euo pipefail
                     |i=${10 * jobDuration.toSeconds}
                     |while [ $$i -ge 0 ]; do sleep 0.1; done
                     |""".stripMargin,
                timeout = Some(timeout)),
        Workflow.of:
          If(expr("!timedOut || $returnCode == 0"),
            Workflow.of:
              Fail(Some(StringConstant:
                "💥 !timedOut || $returnCode == 0"))))))

    withItem(workflow): workflow =>
      val t = now
      val orderId = OrderId("RETURN-CODE")
      val events = controller.runOrder:
        FreshOrder(orderId, workflowPath = workflow.path)
      .map(_.value)
      val elapsed = t.elapsed
      logger.info(elapsed.pretty)
      assert(elapsed >= timeout && elapsed < jobDuration / 2)
      assert(events ==
        Vector(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.TimedOut(OrderOutcome.Failed(Map(
            "returnCode" -> NumberValue(sigtermReturnCode))))),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderMoved(Position(1)),
          OrderDetachable,
          OrderDetached,
          OrderFinished()))
      assert(controllerState.idToOrder(orderId).historicOutcomes ==
        Vector(
          HistoricOutcome(Position(0) / "try+0" % 0, OrderOutcome.TimedOut(OrderOutcome.Failed(Map(
            "returnCode" -> NumberValue(sigtermReturnCode))))),
          HistoricOutcome(Position(0) / "catch+0" % 0, OrderOutcome.Caught)))


object JobTimeoutTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val jobDuration = 10.s
  private val timeout = 200.ms
  private val sigtermReturnCode = if isWindows then 1 else 128 + SIGTERM.number
  private val logger = Logger[this.type]
