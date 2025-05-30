package js7.tests

import cats.effect.IO
import fs2.Stream
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderCancelled, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ExecuteInParallelTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalactic.source
import scala.concurrent.duration.{Deadline, Duration}

final class ExecuteInParallelTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil
  override protected val controllerConfig = config"""
    js7.journal.slow-check-state = off
    js7.journal.log.info-events = []
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """
  override protected def agentConfig = config"""
    js7.journal.slow-check-state = off
    js7.journal.log.info-events = []
    js7.job.execution.signed-script-injection-allowed = on
    """

  "Execute a job many times in parallel" in:
    // Virtual threads will block with more than 128 processes (256 read operations) !!!
    val n = 128 + 1
    val workflow = Workflow.of:
      Execute.Anonymous(WorkflowJob(
        agentPath,
        ShellScriptExecutable:
          """echo STARTED
            |sleep 99999
            |""".stripMargin,
        processLimit = n))

    withItem(workflow): workflow=>
      val t = Deadline.now
      val orderIds = (1 to n).map(i => OrderId(s"#2024-12-09#$i"))
      info_(s"Starting $n shell processes")
      controller.api.addOrders:
        Stream.iterable(orderIds).map:
          FreshOrder(_, workflow.path, deleteWhenTerminated = true)
      .map(_.orThrow)
      .productR:
        IO.blocking:
          info_(f"✔️  ${t.elapsed.pretty}%-6s $n orders added")
          for _ <- 1 to n do
            controller.eventWatch.awaitNext[OrderStdoutWritten]()
          info_(f"✔️  ${t.elapsed.pretty}%-6s $n shell processes are running")
      .productR:
        controller.api.executeCommand:
          CancelOrders(orderIds, CancellationMode.kill(immediately = true))
        .map(_.orThrow)
      .productR:
        IO.blocking:
          for _ <- 1 to n do
            controller.eventWatch.awaitNext[OrderCancelled]()
          info_(f"✔️  ${t.elapsed.pretty}%-6s $n shell processes terminated")
      .timed.map: (duration, _) =>
        val msg = itemsPerSecondString(duration, n, "orders")
        info_(msg)
      .await(if isIntelliJIdea then Duration.Inf else 99.s)

  private def info_(msg: String): Unit =
    note(s"ExecuteInParallelTest $msg")
    logger.info(msg)


object ExecuteInParallelTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
