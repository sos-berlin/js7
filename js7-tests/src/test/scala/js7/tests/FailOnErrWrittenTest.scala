package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderProcessed, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.FailOnErrWrittenTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class FailOnErrWrittenTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)

  "JobResourcePath" in {
    val orderId = OrderId("ORDER")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, Map(
      "A" -> StringValue("A OF ORDER")
    ))).await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.eventsByKey[OrderProcessed](orderId) == Seq(
      OrderProcessed(Outcome.succeededRC0),
      OrderProcessed(Outcome.succeededRC0),
      OrderProcessed(Outcome.Failed(Some("The job's error channel: ERROR"))),
    ))
  }
}

object FailOnErrWrittenTest
{
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute.Anonymous(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo ERROR >&2
            |""".stripMargin))),
      Execute.Anonymous(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo SUCCESS
            |""".stripMargin),
        failOnErrWritten = true)),
      Execute.Anonymous(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo IGNORED >&2
            |echo ERROR >&2
            |""".stripMargin),
        failOnErrWritten = true))))
}
