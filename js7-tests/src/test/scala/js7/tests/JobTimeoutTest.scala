package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.NumberValue
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.JobTimeoutTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.duration.Deadline.now

final class JobTimeoutTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  "timeout" in {
    // Warm-up
    controller.runOrder(
      FreshOrder(OrderId("WARM-UP"), workflowPath = workflow.path, deleteWhenTerminated = true))

    val t = now
    val events = controller.runOrder(FreshOrder(OrderId("ORDER"), workflowPath = workflow.path))
      .map(_.value)
    val elapsed = t.elapsed
    logger.info(elapsed.pretty)
    assert(elapsed >= timeout && elapsed < jobDuration / 2)
    assert(events ==
      Vector(
        OrderAdded(workflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.TimedOut(Outcome.Failed(Map(
          "returnCode" -> NumberValue(if isWindows then 1 else 128 + SIGTERM.number))))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))))
  }
}

object JobTimeoutTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val jobDuration = 10.s
  private val timeout = 200.ms
  private val logger = Logger[this.type]

  private def workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL", Seq(
    Execute(WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        if isWindows then
           s"""@echo off
             |ping -n ${jobDuration.toSeconds + 1} 127.0.0.1 >nul
             |""".stripMargin
        else s"""
         |#!/usr/bin/env bash
         |i=${10 * jobDuration.toSeconds}
         |while [ $$i -ge 0 ]; do sleep 0.1; done
         |""".stripMargin),
      timeout = Some(timeout)))))
}
