package js7.tests.filewatch

import java.nio.file.Files.{createDirectory, delete, exists}
import js7.agent.scheduler.order.FileWatchManager
import js7.base.configutils.Configs._
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline.now
import js7.base.problem.Checked._
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.data.Problems.CannotRemoveWatchingOrderProblem
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{CancelOrders, RemoveOrdersWhenTerminated}
import js7.data.event.EventRequest
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemDestroyed, ItemDestructionMarked, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation.DeleteSimple
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.item.{InventoryItemEvent, ItemRevision}
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderCancellationMarkedOnAgent, OrderFinished, OrderProcessingStarted, OrderRemoved}
import js7.data.order.{OrderId, Outcome}
import js7.data.orderwatch.OrderWatchEvent.ExternalOrderVanished
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.filewatch.FileWatchTest._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.catnap.Semaphore
import monix.eval.Task
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

  private val watchDirectory = directoryProvider.agents(0).dataDir / "tmp/files"
  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    aAgentPath,
    watchDirectory.toString)

  private def fileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(fileWatch, filename).get.orThrow

  private lazy val waitingWatchDirectory = directoryProvider.agents(0).dataDir / "tmp/files-waiting"
  private lazy val waitingFileWatch = FileWatch(
    OrderWatchPath("WAITING-WATCH"),
    waitingWorkflow.path,
    aAgentPath,
    waitingWatchDirectory.toString)

  def watchedFileToOrderId(filename: String): OrderId =
    FileWatchManager.relativePathToOrderId(waitingFileWatch, filename).get.orThrow

  "referencedItemPaths" in {
    assert(fileWatch.referencedItemPaths.toSet == Set(aAgentPath, workflow.path))
  }

  "Start with existing file" in {
    createDirectory(watchDirectory)
    createDirectory(waitingWatchDirectory)
    val file = watchDirectory / "1"
    val orderId = fileToOrderId("1")
    file := ""
    controllerApi.updateUnsignedSimpleItems(Seq(fileWatch, waitingFileWatch)).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](_.event.key == fileWatch.path)
    controller.eventWatch.await[ItemAttached](_.event.key == waitingFileWatch.path)
    controller.eventWatch.await[OrderRemoved](_.key == orderId)
    assert(!exists(file))
  }

  "Add a file" in {
    val file = watchDirectory / "2"
    val orderId = fileToOrderId("2")
    file := ""
    controller.eventWatch.await[OrderRemoved](_.key == orderId)
    assert(!exists(file))
  }

  "Add many files, forcing an overflow" in {
    val since = now
    val filenames = (1 to 1).map(_.toString).toVector
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
      for (f <- files) watchDirectory / f := ""
      sleep(10.ms)
    }
    whenAllRemoved.await(99.s)
    assert(watchDirectory.directoryContents.isEmpty)
    logger.info(itemsPerSecondString(since.elapsed, filenames.size, "files"))
  }

  "RemoveOrdersWhenTerminated is rejected" in {
    val file = waitingWatchDirectory / "REMOVE"
    val orderId = watchedFileToOrderId("REMOVE")
    file := ""
    controller.eventWatch.await[OrderProcessingStarted](_.key == orderId)

    assert(controllerApi.executeCommand(RemoveOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotRemoveWatchingOrderProblem(orderId)))

    semaphore.flatMap(_.release).runSyncUnsafe()
    controller.eventWatch.await[OrderFinished](_.key == orderId)
    intercept[TimeoutException] {
      controller.eventWatch.await[OrderRemoved](_.key == orderId, timeout = 100.ms)
    }

    delete(file)
    val vanished = controller.eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = controller.eventWatch.await[OrderRemoved](_.key == orderId).head
    assert(vanished.timestamp <= removed.timestamp)
  }

  "CancelOrder does not remove the order until the file has vanished" in {
    val file = waitingWatchDirectory / "CANCEL"
    val orderId = watchedFileToOrderId("CANCEL")
    file := ""
    controller.eventWatch.await[OrderProcessingStarted](_.key == orderId)

    controllerApi.executeCommand(CancelOrders(orderId :: Nil)).await(99.s).orThrow
    controller.eventWatch.await[OrderCancellationMarkedOnAgent](_.key == orderId)

    semaphore.flatMap(_.release).runSyncUnsafe()
    controller.eventWatch.await[OrderFinished](_.key == orderId)
    intercept[TimeoutException] {
      controller.eventWatch.await[OrderRemoved](_.key == orderId, timeout = 100.ms)
    }

    assert(controllerApi.executeCommand(RemoveOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotRemoveWatchingOrderProblem(orderId)))

    delete(file)
    val vanished = controller.eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = controller.eventWatch.await[OrderRemoved](_.key == orderId).head
    assert(vanished.timestamp < removed.timestamp)
  }

  private var itemRevision = ItemRevision(0)

  "Add same FileWatch again" in {
    for (i <- 1 to 10) withClue(s"#$i") {
      itemRevision = itemRevision.next
      val eventId = controller.eventWatch.lastAddedEventId
      controllerApi.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
      controller.eventWatch.await[ItemAttached](after = eventId)
      assert(controller.eventWatch.keyedEvents[InventoryItemEvent](after = eventId) ==
        Seq(
          NoKey <-: UnsignedSimpleItemChanged(fileWatch.copy(itemRevision = Some(itemRevision))),
          NoKey <-: ItemAttachable(fileWatch.path, aAgentPath),
          NoKey <-: ItemAttached(fileWatch.path, Some(itemRevision), aAgentPath)))
    }
  }

  "Change Agent" in {
    itemRevision = itemRevision.next
    val eventId = controller.eventWatch.lastAddedEventId
    val changedFileWatch = fileWatch.copy(agentPath = bAgentPath)
    controllerApi.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
    controller.eventWatch.await[ItemAttached](after = eventId)
    assert(controller.eventWatch.keyedEvents[InventoryItemEvent](after = eventId) ==
      Seq(
        NoKey <-: UnsignedSimpleItemChanged(changedFileWatch.copy(itemRevision = Some(itemRevision))),
        NoKey <-: ItemDetachable(fileWatch.path, aAgentPath),
        NoKey <-: ItemDetached(fileWatch.path, aAgentPath),
        NoKey <-: ItemAttachable(fileWatch.path, bAgentPath),
        NoKey <-: ItemAttached(fileWatch.path, Some(itemRevision), bAgentPath)))
  }

  "Delete a FileWatch" in {
    val eventId = controller.eventWatch.lastAddedEventId
    assert(controllerApi.updateItems(Observable(
      DeleteSimple(fileWatch.path),
      DeleteSimple(waitingFileWatch.path))
    ).await(99.s) == Right(Completed))
    controller.eventWatch.await[ItemDestroyed](_.event.key == fileWatch.path, after = eventId)
    val events = controller.eventWatch.keyedEvents[InventoryItemEvent](after = eventId)
      .filter(_.event.key == fileWatch.path)
    assert(events == Seq(
      NoKey <-: ItemDestructionMarked(fileWatch.path),
      NoKey <-: ItemDetachable(fileWatch.path, bAgentPath),
      NoKey <-: ItemDetached(fileWatch.path, bAgentPath),
      NoKey <-: ItemDestroyed(fileWatch.path)))
    sleep(100.ms)   // Wait until controllerState has been updated
    assert(controller.controllerState.await(99.s).allOrderWatchesState.pathToOrderWatchState.isEmpty)
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
      Execute(WorkflowJob(
        aAgentPath,
        InternalExecutable(classOf[SemaphoreJob].getName)))))

  private val semaphore = Semaphore[Task](0).memoize

  final class SemaphoreJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        semaphore.flatMap(_.acquire)
          .as(Outcome.succeeded))
  }
}
