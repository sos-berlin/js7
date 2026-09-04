package js7.tests

import java.nio.file.Files.createTempFile
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.item.VersionId
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.Value.convenience.given
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ExecuteProcessTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectoryProvider}
import scala.concurrent.duration.Deadline
import scala.language.implicitConversions

final class ExecuteProcessTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = agentPath :: Nil
  protected val items = Nil

  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"ORDER-$i"))
  private lazy val argScriptFile = createTempFile("ExecuteTest-arg-", ".cmd")
  private lazy val myReturnCodeScriptFile = createTempFile("ExecuteTest-myExitCode-", ".cmd")

  "No maxWaitForStdouterr" in:
    withItem(
      Workflow.of(WorkflowPath("WORKFLOW"),
        Execute(WorkflowJob(
          agentPath,
          ShellScriptExecutable(
            """#!/usr/bin/env bash
              |set -euo pipefail
              |(trap "" SIGTERM; sleep 0.1; echo "+++ CHILD FINISHED +++") &
              |sleep 0.05
              |""".stripMargin),
          maxWaitForStdouterr = None)))
    ): workflow =>
      val orderId = OrderId("ORDER-WAIT")
      val t = Deadline.now
      runOrder(FreshOrder(orderId, workflow.path))
      assert(t.elapsed >= 100.ms && t.elapsed <= 1.s)
      val events = controller.eventsByKey[OrderEvent](orderId)
      assert(events == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(Some(subagentId)),
        OrderStdoutWritten("+++ CHILD FINISHED +++\n"),
        OrderProcessed(OrderOutcome.Succeeded(Map("returnCode" -> 0))),
        OrderMoved(Position(1), None),
        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))

  "maxWaitForStdouterr" in:
    withItem(
      Workflow.of(WorkflowPath("WORKFLOW"),
        Execute(WorkflowJob(
          agentPath,
          ShellScriptExecutable(
            """#!/usr/bin/env bash
              |set -euo pipefail
              |(trap "" SIGTERM; sleep 3; echo "+++ CHILD FINISHED +++") &
              |sleep 0.1
              |""".stripMargin),
          maxWaitForStdouterr = Some(500.ms))))
    ): workflow =>
      val orderId = OrderId("ORDER-DONT-WAIT")
      val t = Deadline.now
      runOrder(FreshOrder(orderId, workflow.path))
      assert(t.elapsed >= 500.ms && t.elapsed <= 3.s)
      val events = controller.eventsByKey[OrderEvent](orderId)
      assert(events == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(Some(subagentId)),
        OrderProcessed(OrderOutcome.Succeeded(Map("returnCode" -> 0))),
        OrderMoved(Position(1), None),
        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))


object ExecuteProcessTest:

  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
