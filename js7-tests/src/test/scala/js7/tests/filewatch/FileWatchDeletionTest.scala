package js7.tests.filewatch

import fs2.Stream
import java.io.File
import java.nio.file.Files.createDirectory
import java.nio.file.Paths
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.ItemOperation.DeleteSimple
import js7.data.order.OrderEvent.{OrderDeleted, OrderFinished, OrderProcessingStarted}
import js7.data.order.OrderId
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.scopes.EnvScope
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchDeletionTest.*
import js7.tests.jobs.{DeleteFileJob, SemaphoreJob}
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}

final class FileWatchDeletionTest
extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil // No Workflow, because we add Workflow and FileWatch in same operation

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  // Calculate directory path from an environment variable
  private val watchPrefix = (directoryProvider.agentEnvs(0).dataDir / "work").toString + File.separator
  private val watchDirectory = Paths.get(watchPrefix + "files")

  private val envName = getClass.getName
  EnvScope.putForTest(envName, "files")
  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    agentPath,
    expr(s"${StringConstant.quote(watchPrefix)} ++ env('$envName')"))

  override def beforeAll(): Unit =
    super.beforeAll()

    locally:
      createDirectory(watchDirectory)
      updateItems(workflow, fileWatch)
      eventWatch.awaitNext[ItemAttached](_.event.key == fileWatch.path)

  "Run an Order" in:
    val name = "NORMAL"
    val orderId = OrderId(s"file:TEST-WATCH:$name")
    watchDirectory / name := ""
    TestJob.continue()
    eventWatch.awaitNext[OrderFinished](_.key == orderId)

  "JS-2159 Delete a FileWatch while an Order is still running" in:
    val name = "AFTER-DELETION"
    val orderId = OrderId(s"file:TEST-WATCH:$name")
    watchDirectory / name := ""
    eventWatch.awaitNext[OrderProcessingStarted](_.key == orderId)

    /// Delete the FileWatch ///
    controller.api.updateItems(Stream(DeleteSimple(fileWatch.path))).await(99.s).orThrow
    TestJob.continue()
    eventWatch.awaitNext[OrderFinished](_.key == orderId)

    /// Without FileWatch, the file is considered deleted ///
    eventWatch.awaitNext[OrderDeleted](_.key == orderId)


object FileWatchDeletionTest:
  private val agentPath = AgentPath("AGENT-A")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      TestJob.execute(agentPath),
      DeleteFileJob.execute(agentPath)))

  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
