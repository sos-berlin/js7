package js7.tests.filewatch

import java.io.File
import java.nio.file.Files.{createDirectory, delete, exists}
import java.nio.file.Paths
import js7.agent.scheduler.order.FileWatchManager
import js7.base.configutils.Configs._
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.Checked._
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.data.Problems.{CannotDeleteWatchingOrderProblem, ItemIsStillReferencedProblem}
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.event.EventRequest
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemDeleted, ItemDeletionMarked, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.item.{InventoryItemEvent, ItemRevision, VersionId}
import js7.data.order.OrderEvent.{OrderCancellationMarkedOnAgent, OrderDeleted, OrderFinished, OrderProcessingStarted}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderArised, ExternalOrderVanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.scopes.EnvScope
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchTest._
import js7.tests.jobs.{DeleteFileJob, SemaphoreJob}
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.TimeoutException

final class FileWatchTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(aAgentPath, bAgentPath)
  protected val items = Seq(workflow, waitingWorkflow)

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  // Calculate directory path from an environment variable
  private val watchPrefix = (directoryProvider.agents(0).dataDir / "work").toString + File.separator
  private val watchDirectory = Paths.get(watchPrefix + "files")
  private val envName = getClass.getName
  EnvScope.putForTest(envName, "files")
  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    aAgentPath,
    expr(s"${StringConstant.quote(watchPrefix)} ++ env('$envName')"))

  private def fileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(fileWatch, filename).get.orThrow

  private lazy val waitingWatchDirectory = directoryProvider.agents(0).dataDir / "work/files-waiting"
  private lazy val waitingFileWatch = FileWatch(
    OrderWatchPath("WAITING-WATCH"),
    waitingWorkflow.path,
    aAgentPath,
    StringConstant(waitingWatchDirectory.toString))

  override def beforeAll(): Unit = {
    super.beforeAll()

    locally {
      // Create FileWatch and test with an already waiting Order
      createDirectory(watchDirectory)
      createDirectory(waitingWatchDirectory)
      val file = watchDirectory / "1"
      val orderId = fileToOrderId("1")
      file := ""
      controllerApi.updateUnsignedSimpleItems(Seq(fileWatch, waitingFileWatch)).await(99.s).orThrow
      eventWatch.await[ItemAttached](_.event.key == fileWatch.path)
      eventWatch.await[ItemAttached](_.event.key == waitingFileWatch.path)
      eventWatch.await[OrderDeleted](_.key == orderId)
      assert(!exists(file))
    }
  }

  def watchedFileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(waitingFileWatch, filename).get.orThrow

  "referencedItemPaths" in {
    assert(fileWatch.referencedItemPaths.toSet == Set(aAgentPath, workflow.path))
  }

  "Start with existing file" in {
  }

  "Add a file" in {
    val file = watchDirectory / "2"
    val orderId = fileToOrderId("2")
    file := ""
    eventWatch.await[OrderDeleted](_.key == orderId)
    assert(!exists(file))
  }

  "Add many files, forcing an overflow" in {
    val since = now
    val filenames = (1 to 1).map(_.toString).toVector
    val orderIds = filenames.map(fileToOrderId).toSet
    val whenAllRemoved = eventWatch
      .observe(EventRequest.singleClass[OrderDeleted](
        after = eventWatch.lastAddedEventId,
        timeout = Some(88.s)))
      .scan(orderIds)((set, stamped) => set - stamped.value.key)
      .dropWhile(_.nonEmpty)
      .headL
      .runToFuture
    for (files <- filenames.grouped(100)) {
      for (f <- files) watchDirectory / f := ""
      sleep(10.ms)
    }
    whenAllRemoved.await(99.s)
    assert(watchDirectory.directoryContents.isEmpty)
    logger.info(itemsPerSecondString(since.elapsed, filenames.size, "files"))
  }

  "DeleteOrdersWhenTerminated is rejected" in {
    val file = waitingWatchDirectory / "REMOVE"
    val orderId = watchedFileToOrderId("REMOVE")
    file := ""
    eventWatch.await[OrderProcessingStarted](_.key == orderId)

    assert(controllerApi.executeCommand(DeleteOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotDeleteWatchingOrderProblem(orderId)))

    TestJob.continue()
    eventWatch.await[OrderFinished](_.key == orderId)
    intercept[TimeoutException] {
      eventWatch.await[OrderDeleted](_.key == orderId, timeout = 100.ms)
    }

    delete(file)
    val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = eventWatch.await[OrderDeleted](_.key == orderId).head
    assert(vanished.timestamp <= removed.timestamp)
  }

  "CancelOrder does not delete the order until the file has vanished" in {
    val file = waitingWatchDirectory / "CANCEL"
    val orderId = watchedFileToOrderId("CANCEL")
    file := ""
    eventWatch.await[OrderProcessingStarted](_.key == orderId)

    controllerApi.executeCommand(CancelOrders(orderId :: Nil)).await(99.s).orThrow
    eventWatch.await[OrderCancellationMarkedOnAgent](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderFinished](_.key == orderId)
    intercept[TimeoutException] {
      eventWatch.await[OrderDeleted](_.key == orderId, timeout = 100.ms)
    }

    assert(controllerApi.executeCommand(DeleteOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotDeleteWatchingOrderProblem(orderId)))

    delete(file)
    val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = eventWatch.await[OrderDeleted](_.key == orderId).head
    assert(vanished.timestamp <= removed.timestamp)
  }

  private var itemRevision = ItemRevision(0)

  "Change FileWatch while an order is running" in {
    TestJob.reset()
    val longFile = waitingWatchDirectory / "AGAIN-LONG"
    val longOrderId = watchedFileToOrderId("AGAIN-LONG")
    longFile := ""
    TestJob.continue()

    for (i <- 1 to 2) withClue(s"#$i") {
      itemRevision = itemRevision.next
      val eventId = eventWatch.lastAddedEventId
      val changedFileWatch = waitingFileWatch.copy(delay = i.ms/*little change*/)
      controllerApi.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
      eventWatch.await[ItemAttached](after = eventId)
      assert(eventWatch.keyedEvents[InventoryItemEvent](after = eventId) ==
        Seq(
          NoKey <-: UnsignedSimpleItemChanged(changedFileWatch.copy(itemRevision = Some(itemRevision))),
          NoKey <-: ItemAttachable(changedFileWatch.path, aAgentPath),
          NoKey <-: ItemAttached(changedFileWatch.path, Some(itemRevision), aAgentPath)))

      val iFile = waitingWatchDirectory / s"AGAIN-$i"
      val iOrderId = watchedFileToOrderId(s"AGAIN-$i")
      iFile := ""
      TestJob.continue()
      eventWatch.await[OrderFinished](_.key == iOrderId)

      delete(iFile)
      val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
      val removed = eventWatch.await[OrderDeleted](_.key == iOrderId).head
      assert(vanished.timestamp <= removed.timestamp)
    }

    eventWatch.await[OrderFinished](_.key == longOrderId)
    intercept[TimeoutException] {
      eventWatch.await[OrderDeleted](_.key == longOrderId, timeout = 100.ms)
    }
    delete(longFile)
    val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = eventWatch.await[OrderDeleted](_.key == longOrderId).head
    assert(vanished.timestamp <= removed.timestamp)
  }

  "Change directory" in {
    TestJob.reset()
    TestJob.continue(5)

    // A file only in the old directory
    val singletonFile = waitingWatchDirectory / "CHANGE-DIRECTORY-SINGLETON"
    val singletonOrderId = watchedFileToOrderId("CHANGE-DIRECTORY-SINGLETON")
    singletonFile := ""

    // Same filename in old and new directory
    val originalBothFile = waitingWatchDirectory / "CHANGE-DIRECTORY-BOTH"
    val bothOrderId = watchedFileToOrderId("CHANGE-DIRECTORY-BOTH")
    originalBothFile := ""

    eventWatch.await[OrderProcessingStarted](_.key == singletonOrderId)
    eventWatch.await[OrderProcessingStarted](_.key == bothOrderId)

    withTemporaryDirectory() { newDirectory =>
      val bothFile = newDirectory / "CHANGE-DIRECTORY-BOTH"
      bothFile := ""

      // A file only in the newDirectory
      val newFile = newDirectory / "CHANGE-DIRECTORY-NEW"
      val newOrderId = watchedFileToOrderId("CHANGE-DIRECTORY-NEW")
      newFile := ""

      val eventId = eventWatch.lastAddedEventId

      val changedFileWatch = waitingFileWatch.copy(
        directory = expr(StringConstant.quote(newDirectory.toString)))
      controllerApi.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
      eventWatch.await[ItemAttached](after = eventId)
      eventWatch.await[OrderDeleted](_.key == singletonOrderId, after = eventId)
      eventWatch.await[OrderDeleted](_.key == bothOrderId, after = eventId)

      // File in old directory is ignored
      val oldFile = waitingWatchDirectory / "CHANGE-DIRECTORY-OLD"
      oldFile := ""

      val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
      val removed = eventWatch.await[OrderDeleted](_.key == singletonOrderId).head
      assert(vanished.timestamp <= removed.timestamp)

      // bothOrderId has been started again because its filename duplicates in newDirectory
      eventWatch.await[OrderProcessingStarted](_.key == bothOrderId, after = eventId)
      eventWatch.await[OrderProcessingStarted](_.key == newOrderId, after = eventId)
      eventWatch.await[OrderDeleted](_.key == bothOrderId, after = eventId)
      eventWatch.await[OrderFinished](_.key == newOrderId, after = eventId)
      delete(newFile)
      eventWatch.await[OrderDeleted](_.key == newOrderId, after = eventId)

      assert(eventWatch
        .keyedEvents[ExternalOrderArised](after = eventId)
        .map(_.event.arguments(FileWatch.FileArgumentName).asString.orThrow)
        .toSet ==
        Set(
          bothFile.toString,
          newFile.toString))

      // Java for MacOS needs 2s to detect a file (maybe oldFile)
      if (isMac) sleep(2500.ms)

      assert(eventWatch
        .keyedEvents[ExternalOrderVanished](after = eventId)
        .map(_.event.externalOrderName)
        .toSet ==
        Set(
          ExternalOrderName(singletonFile.getFileName.toString),
          ExternalOrderName(originalBothFile.getFileName.toString),
          ExternalOrderName(bothFile.getFileName.toString),
          ExternalOrderName(newFile.getFileName.toString)))

      delete(oldFile)
      delete(originalBothFile)
    }

    // Restore waitingFileWatch
    val eventId = eventWatch.lastAddedEventId
    controllerApi.updateUnsignedSimpleItems(Seq(waitingFileWatch)).await(99.s).orThrow
    eventWatch.await[ItemAttached](_.event.key == waitingFileWatch.path, after = eventId)
  }

  "Change Agent" in {
    val eventId = eventWatch.lastAddedEventId
    val changedFileWatch = fileWatch.copy(agentPath = bAgentPath)
    controllerApi.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
    eventWatch.await[ItemAttached](after = eventId)
    assert(eventWatch.keyedEvents[InventoryItemEvent](after = eventId) ==
      Seq(
        NoKey <-: UnsignedSimpleItemChanged(changedFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
        NoKey <-: ItemDetachable(fileWatch.path, aAgentPath),
        NoKey <-: ItemDetached(fileWatch.path, aAgentPath),
        NoKey <-: ItemAttachable(fileWatch.path, bAgentPath),
        NoKey <-: ItemAttached(fileWatch.path, Some(ItemRevision(1)), bAgentPath)))
  }

  "Deleting the Workflow referenced by the FileWatch is rejected" in {
    assert(controllerApi.updateItems(Observable(
      AddVersion(VersionId("TRY-DELETE")),
      RemoveVersioned(workflow.path)
    )).await(99.s) ==
      Left(ItemIsStillReferencedProblem(workflow.path, fileWatch.path)))
  }

  "Delete a FileWatch" in {
    val eventId = eventWatch.lastAddedEventId
    assert(controllerApi.updateItems(Observable(
      DeleteSimple(fileWatch.path),
      DeleteSimple(waitingFileWatch.path)
    )).await(99.s) == Right(Completed))
    eventWatch.await[ItemDeleted](_.event.key == fileWatch.path, after = eventId)
    val events = eventWatch.keyedEvents[InventoryItemEvent](after = eventId)
      .filter(_.event.key == fileWatch.path)
    assert(events == Seq(
      NoKey <-: ItemDeletionMarked(fileWatch.path),
      NoKey <-: ItemDetachable(fileWatch.path, bAgentPath),
      NoKey <-: ItemDetached(fileWatch.path, bAgentPath),
      NoKey <-: ItemDeleted(fileWatch.path)))
    sleep(100.ms)   // Wait until controllerState has been updated
    assert(controllerState.keyTo(OrderWatchState).isEmpty)
  }
}

object FileWatchTest
{
  private val logger = Logger(getClass)
  private val aAgentPath = AgentPath("AGENT-A")
  private val bAgentPath = AgentPath("AGENT-B")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(DeleteFileJob.execute(aAgentPath)))

  private val waitingWorkflow = Workflow(
    WorkflowPath("WAITING-WORKFLOW") ~ "INITIAL",
    Vector(
      TestJob.execute(aAgentPath)))

  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
}
