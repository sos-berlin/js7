package js7.tests.filewatch

import java.nio.file.Files.{createDirectory, exists}
import js7.agent.scheduler.order.FileWatchManager
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.Checked._
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.data.agent.AgentId
import js7.data.event.EventRequest
import js7.data.item.SimpleItemEvent.SimpleItemAttached
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.OrderRemoved
import js7.data.order.OrderId
import js7.data.orderwatch.FileWatch.FileArgumentName
import js7.data.orderwatch.{FileWatch, OrderWatchId}
import js7.data.value.expression.Expression.{NamedValue, ObjectExpression}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchTest._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class FileWatchTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(aAgentId)
  protected val versionedItems = Seq(workflow)
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  private val sourceDirectory = directoryProvider.agents(0).dataDir / "tmp/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchId("TEST-WATCH"),
    workflow.path,
    aAgentId,
    sourceDirectory.toString)

  private def fileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(fileWatch, filename).get.orThrow

  "Start with existing file" in {
    createDirectory(sourceDirectory)
    val file = sourceDirectory / "1"
    val orderId = fileToOrderId("1")
    file := ""
    controller.updateSimpleItemsAsSystemUser(Seq(fileWatch)).await(99.s).orThrow
    controller.eventWatch.await[SimpleItemAttached](_.event.id == fileWatch.id)
    controller.eventWatch.await[OrderRemoved](_.key == orderId)
    assert(!exists(file))
  }

  "Add a file" in {
    val file = sourceDirectory / "2"
    val orderId = fileToOrderId("2")
    file := ""
    controller.eventWatch.await[OrderRemoved](_.key == orderId)
    assert(!exists(file))
  }

  "Add many files, forcing an overflow" in {
    val since = now
    val filenames = (1 to 1000).map(_.toString).toVector
    val orderIds = filenames.map(fileToOrderId).toSet
    val whenAllRemoved = controller.eventWatch
      .observe(EventRequest.singleClass[OrderRemoved](
        after = controller.eventWatch.lastAddedEventId,
        timeout = Some(88.s)))
      .scan(orderIds)((set, stamped) => set - stamped.value.key)
      .dropWhile(_.nonEmpty)
      .headL
      .runToFuture
    for (files <- filenames.grouped(100)) {
      for (f <- files) sourceDirectory / f := ""
      sleep(10.ms)
    }
    whenAllRemoved.await(99.s)
    assert(sourceDirectory.directoryContents.isEmpty)
    logger.info(itemsPerSecondString(since.elapsed, filenames.size, "files"))
  }
}

object FileWatchTest
{
  private val logger = Logger(getClass)
  private val aAgentId = AgentId("AGENT-A")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(aAgentId,
        InternalExecutable(
          classOf[DeleteFileJob].getName,
          arguments = ObjectExpression(Map("file" -> NamedValue(FileArgumentName)))),
        taskLimit = 100))))
}
