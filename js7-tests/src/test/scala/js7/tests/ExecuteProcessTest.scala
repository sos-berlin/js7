package js7.tests

import java.nio.file.Files.createTempFile
import js7.base.configutils.Configs.*
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode.FreshOrStarted
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.item.VersionId
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.Value.convenience.{given_Conversion_Int_NumberValue, given}
import js7.data.value.expression.Expression.{expr, exprFun}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ForkList}
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
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.order.stdout-stderr.delay = 1ms
    js7.order.stdout-stderr.commit-delay = 0ms
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
              |(trap "" SIGTERM; sleep 0.4; echo "+++ CHILD FINISHED +++") &
              |sleep 0.2
              |""".stripMargin),
          maxWaitForStdouterr = None)))
    ): workflow =>
      val orderId = OrderId("ORDER-WAIT")
      runOrder(FreshOrder(orderId, workflow.path))
      val events = controller.eventsByKey[OrderEvent](orderId)
      // May fail when child process starts too late due to heavy load
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
              |( trap "" SIGTERM
              |  echo "+++ CHILD +++"
              |  sleep 0.2
              |  echo "+++ CHILD 2 +++"
              |  sleep 0.5
              |  echo "+++ CHILD FINISHED +++"
              |) &
              |sleep 0.2
              |""".stripMargin),
          maxWaitForStdouterr = Some(200.ms))))
    ): workflow =>
      val orderId = OrderId("ORDER-WAIT-SHORTLY")
      val eventId = controller.lastAddedEventId
      runOrder(FreshOrder(orderId, workflow.path))
      val stamped = controller.eventWatch.allStamped[OrderEvent](after = eventId)
        .filter(stamped => stamped.value.key == orderId)

      val startedAt = stamped.find(_.value.event.isInstanceOf[OrderProcessingStarted]).get.timestamp
      val processedAt = stamped.find(_.value.event.isInstanceOf[OrderProcessed]).get.timestamp
      val duration = processedAt - startedAt
      assert(duration >= 200.ms && duration <= 1.s)

      // May fail when child process starts too late due to heavy load
      assert(stamped.map(_.value.event) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(Some(subagentId)),
        OrderStdoutWritten("+++ CHILD +++\n"),
        OrderStdoutWritten("+++ CHILD 2 +++\n"),
        OrderProcessed(OrderOutcome.Succeeded(Map("returnCode" -> 0))),
        OrderMoved(Position(1), None),
        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))

      // Be sure that no OrderStdoutWritten event is emitted after the OrderProcessed event
      // Otherwise, the Subagent would crash here due to .orThrow after persist operation.
      sleepUntil(processedAt + 1.s)


  "Multiple processes" in:
    val n = 100
    val childSleep = if isIntelliJIdea then 10.s else 77.s
    withItem(
      Workflow.of(WorkflowPath("WORKFLOW"),
        ForkList(
          children = expr"$$children",
          childToId = exprFun"o => $$o",
          childToArguments = exprFun"o => {}",
          Workflow.of:
            Execute(WorkflowJob(
              agentPath,
              ShellScriptExecutable(
                s"""#!/usr/bin/env bash
                  |set -euo pipefail
                  |( #trap "" SIGTERM
                  |  echo "+++ CHILD +++"
                  |  sleep ${childSleep.toDecimalString}
                  |  echo "+++ CHILD FINISHED +++"
                  |) &
                  |sleep 1
                  |""".stripMargin),
              processLimit = n,
              maxWaitForStdouterr = Some(200.ms)))))
    ): workflow =>
      val t = Deadline.now
      runOrder(FreshOrder(OrderId("FORK"), workflow.path, Map(
        "children" -> (1 to n))))
      assert(t.elapsed < childSleep)


  "Cancel while waiting for stdout of background child process" in:
    withItem(
      Workflow.of(WorkflowPath("WORKFLOW"),
        Execute(WorkflowJob(
          agentPath,
          ShellScriptExecutable(
            """#!/usr/bin/env bash
              |set -euo pipefail
              |(trap "" SIGTERM; sleep 0.4; echo +++ CHILD +++; sleep 999) &
              |sleep 0.2
              |""".stripMargin))))
    ): workflow =>
      val orderId = OrderId("ORDER-WAIT-KILL")
      val eventId = controller.lastAddedEventId
      addOrder(orderId, workflow.path)
      controller.awaitNextKey[OrderStdoutWritten](orderId, after = eventId)
      execCmd:
        CancelOrders(orderId :: Nil, FreshOrStarted(Some(SIGTERM)))
      controller.awaitNextKey[OrderTerminated](orderId, after = eventId)


object ExecuteProcessTest:

  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
