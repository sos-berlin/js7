package js7.tests.filewatch

import com.typesafe.config.ConfigFactory
import fs2.Stream
import java.io.File
import java.nio.file.Files.{createDirectories, createDirectory, delete, exists}
import java.nio.file.Paths
import js7.base.configutils.Configs.*
import js7.base.generic.Completed
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.system.OperatingSystem.isMac
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.Timestamp
import js7.data.Problems.{CannotDeleteWatchingOrderProblem, ItemIsStillReferencedProblem}
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.event.EventRequest
import js7.data.event.KeyedEvent.NoKey
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttached, ItemDeleted, ItemDeletionMarked, ItemDetachable, ItemDetached}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemChanged
import js7.data.item.{InventoryItemEvent, ItemRevision, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderCancellationMarkedOnAgent, OrderDeleted, OrderExternalVanished, OrderFinished, OrderProcessingStarted}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderAppeared, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.HasOrder
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.plan.PlanId
import js7.data.value.StringValue
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.scopes.EnvScope
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.filewatch.FileWatchTest.*
import js7.tests.jobs.{DeleteFileJob, SemaphoreJob}
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Deadline

final class FileWatchTest
extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(aAgentPath, bAgentPath)
  protected val items = Nil // No Workflow, because we add Workflow and FileWatch in same operation

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """.withFallback:
      if isMac then
        // Maybe since Java 24, directory changes are detected only when polling.
        // So we shorten the default 60s
        config"""js7.directory-watch.poll-timeout = 10s"""
      else
        ConfigFactory.empty()

  // Calculate directory path from an environment variable
  private val watchPrefix = (directoryProvider.agentEnvs(0).dataDir / "work").toString + File.separator
  private val watchDirectory = Paths.get(watchPrefix + "files")
  private val envName = getClass.getName
  EnvScope.putForTest(envName, "files")
  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    aAgentPath,
    expr(s"${StringConstant.quote(watchPrefix)} ++ env('$envName')"))

  private def externalToOrderId(externalOrderName: ExternalOrderName): OrderId =
    val (orderId, planId) = fileWatch.externalToOrderAndPlanId(externalOrderName, None, Timestamp.now).orThrow
    assert(planId == PlanId.Global)
    orderId

  private def waitingFileToOrderId(externalOrderName: ExternalOrderName): OrderId =
    val (orderId, planId) =     waitingFileWatch.externalToOrderAndPlanId(externalOrderName, None, Timestamp.now).orThrow
    assert(planId == PlanId.Global)
    orderId

  private lazy val waitingWatchDirectory = directoryProvider.agentEnvs(0).dataDir / "work/files-waiting"
  private lazy val waitingFileWatch = FileWatch(
    OrderWatchPath("WAITING-WATCH"),
    waitingWorkflow.path,
    aAgentPath,
    StringConstant(waitingWatchDirectory.toString))

  override def beforeAll(): Unit =
    super.beforeAll()
    // Create FileWatch and test with an already waiting Order
    createDirectory(watchDirectory)
    createDirectory(waitingWatchDirectory)
    updateItems(workflow, waitingWorkflow, fileWatch, waitingFileWatch)
    eventWatch.await[ItemAttached](_.event.key == fileWatch.path)
    eventWatch.await[ItemAttached](_.event.key == waitingFileWatch.path)

  "referencedItemPaths" in:
    assert(fileWatch.referencedItemPaths.toSet == Set(aAgentPath, workflow.path))

  "Start with existing file; check Workflow's Order declarations" in:
    // Create FileWatch and test with an already waiting Order
    val myDirectory = Paths.get(watchPrefix + "existing")
    val file = myDirectory / "1"
    createDirectories(myDirectory)
    file := ""

    val myFileWatch = FileWatch(
      OrderWatchPath("EXISTING"),
      waitingWorkflow.path,
      aAgentPath,
      StringConstant(myDirectory.toString))
    updateItems(myFileWatch)

    val (orderId, PlanId.Global) =
      myFileWatch.externalToOrderAndPlanId(ExternalOrderName("1"), None, Timestamp.now).orThrow: @unchecked
    eventWatch.await[OrderProcessingStarted](_.key == orderId)
    assert(controllerState.idToOrder(orderId).namedValues(waitingWorkflow).toMap == Map(
      "file" -> StringValue(file.toString),
      "DEFAULT" -> StringValue("DEFAULT-VALUE")))

    TestJob.continue()
    eventWatch.await[OrderFinished](_.key == orderId)

    delete(file)
    eventWatch.await[OrderDeleted](_.key == orderId)

    controller.api.updateItems(Stream(DeleteSimple(myFileWatch.path))).await(99.s).orThrow

  "Add a file" in:
    val file = watchDirectory / "2"
    val orderId = externalToOrderId(ExternalOrderName("2"))
    file := ""
    eventWatch.await[OrderDeleted](_.key == orderId)
    assert(!exists(file))

  "Add many files, forcing an overflow" in:
    val since = Deadline.now
    val filenames = (1 to 1).map(_.toString).toVector
    val orderIds = filenames.map(ExternalOrderName(_)).map(externalToOrderId).toSet
    val whenAllRemoved = eventWatch
      .stream(EventRequest.singleClass[OrderDeleted](
        after = eventWatch.lastAddedEventId,
        timeout = Some(88.s)))
      .scan(orderIds)((set, stamped) => set - stamped.value.key)
      .dropWhile(_.nonEmpty)
      .head.compile.last
      .unsafeToFuture()
    for files <- filenames.grouped(100) do
      for f <- files do watchDirectory / f := ""
      sleep(10.ms)
    whenAllRemoved.await(99.s)
    assert(watchDirectory.directoryContents.isEmpty)
    logger.info(itemsPerSecondString(since.elapsed, filenames.size, "files"))

  "DeleteOrdersWhenTerminated is rejected" in:
    val file = waitingWatchDirectory / "REMOVE"
    val orderId = waitingFileToOrderId(ExternalOrderName("REMOVE"))
    file := ""
    eventWatch.await[OrderProcessingStarted](_.key == orderId)

    assert(controller.api.executeCommand(DeleteOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotDeleteWatchingOrderProblem(orderId)))

    TestJob.continue()
    eventWatch.await[OrderFinished](_.key == orderId)
    intercept[TimeoutException]:
      eventWatch.await[OrderDeleted](_.key == orderId, timeout = 100.ms)

    delete(file)
    val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = eventWatch.await[OrderDeleted](_.key == orderId).head
    assert(vanished.timestamp <= removed.timestamp)

  "CancelOrder does not delete the order until the file has vanished" in:
    val file = waitingWatchDirectory / "CANCEL"
    val orderId = waitingFileToOrderId(ExternalOrderName("CANCEL"))
    file := ""
    eventWatch.await[OrderProcessingStarted](_.key == orderId)

    execCmd(CancelOrders(orderId :: Nil))
    eventWatch.await[OrderCancellationMarkedOnAgent](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderFinished](_.key == orderId)
    intercept[TimeoutException]:
      eventWatch.await[OrderDeleted](_.key == orderId, timeout = 100.ms)

    assert(controller.api.executeCommand(DeleteOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotDeleteWatchingOrderProblem(orderId)))

    delete(file)
    val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = eventWatch.await[OrderDeleted](_.key == orderId).head
    assert(vanished.timestamp <= removed.timestamp)

  private var itemRevision = ItemRevision(0)

  "Change FileWatch while an order is running" in:
    TestJob.reset()
    val longFile = waitingWatchDirectory / "AGAIN-LONG"
    val longOrderId = waitingFileToOrderId(ExternalOrderName("AGAIN-LONG"))
    longFile := ""
    TestJob.continue()

    for i <- 1 to 2 do withClue(s"#$i"):
      itemRevision = itemRevision.next
      val eventId = eventWatch.lastAddedEventId
      val changedFileWatch = waitingFileWatch.copy(delay = i.ms/*little change*/)
      controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
      eventWatch.await[ItemAttached](after = eventId)
      assert(eventWatch.keyedEvents[InventoryItemEvent](after = eventId) ==
        Seq(
          NoKey <-: UnsignedSimpleItemChanged(changedFileWatch.copy(itemRevision = Some(itemRevision))),
          NoKey <-: ItemAttachable(changedFileWatch.path, aAgentPath),
          NoKey <-: ItemAttached(changedFileWatch.path, Some(itemRevision), aAgentPath)))

      val iFile = waitingWatchDirectory / s"AGAIN-$i"
      val iOrderId = waitingFileToOrderId(ExternalOrderName(s"AGAIN-$i"))
      iFile := ""
      TestJob.continue()
      eventWatch.await[OrderFinished](_.key == iOrderId)

      delete(iFile)
      val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
      val removed = eventWatch.await[OrderDeleted](_.key == iOrderId).head
      assert(vanished.timestamp <= removed.timestamp)

    eventWatch.await[OrderFinished](_.key == longOrderId)
    intercept[TimeoutException]:
      eventWatch.await[OrderDeleted](_.key == longOrderId, timeout = 100.ms)
    delete(longFile)
    val vanished = eventWatch.await[ExternalOrderVanished](_.key == waitingFileWatch.path).head
    val removed = eventWatch.await[OrderDeleted](_.key == longOrderId).head
    assert(vanished.timestamp <= removed.timestamp)

  "Change directory" - {
    "Different files in old and new directory" in:
      TestJob.reset()
      TestJob.continue(5)

      // A file only in the old directory
      val singletonName = "CHANGE-DIRECTORY-SINGLETON"
      val singletonFile = waitingWatchDirectory / singletonName
      val singletonOrderId = waitingFileToOrderId(ExternalOrderName(singletonName))
      singletonFile := ""

      eventWatch.await[OrderFinished](_.key == singletonOrderId)
      assert(exists(singletonFile))

      withTemporaryDirectory() { newDirectory =>
        val eventId = eventWatch.lastAddedEventId

        // A file only in the newDirectory
        val newFile = newDirectory / "CHANGE-DIRECTORY-NEW"
        val newOrderId = waitingFileToOrderId(ExternalOrderName("CHANGE-DIRECTORY-NEW"))
        newFile := ""

        /// Change directory ///
        val changedFileWatch = waitingFileWatch.copy(
          directoryExpr = expr(StringConstant.quote(newDirectory.toString)))
        controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
        eventWatch.await[ItemAttached](after = eventId)

        /// Files are considered deleted (due to directory change — they still exists) ///
        eventWatch.await[OrderExternalVanished](_.key == singletonOrderId, after = eventId)
        eventWatch.await[OrderDeleted](_.key == singletonOrderId, after = eventId)
        //eventId = eventWatch.lastAddedEventId

        // File in old directory is ignored
        val oldFile = waitingWatchDirectory / "CHANGE-DIRECTORY-OLD"
        oldFile := ""

        val vanished = eventWatch.await[ExternalOrderVanished](
          _ == waitingFileWatch.path <-: ExternalOrderVanished(ExternalOrderName(singletonName)),
          after = eventId).head
        val deleted = eventWatch
          .await[OrderExternalVanished](_.key == singletonOrderId, after = eventId).head
        assert(vanished.timestamp <= deleted.timestamp)

        // bothOrderId has been started again because its filename duplicates in newDirectory

        eventWatch.await[OrderFinished](_.key == newOrderId, after = eventId)
        delete(newFile)
        eventWatch.await[OrderDeleted](_.key == newOrderId, after = eventId)

        assert(eventWatch
          .keyedEvents[ExternalOrderAppeared](after = eventId)
          .map(_.event.arguments(FileWatch.FileArgumentName).asString.orThrow)
          .toSet ==
          Set(newFile.toString))

        // Java for macOS needs 2s to detect a file (maybe oldFile)
        if isMac then sleep(2500.ms)

        assert(eventWatch
          .keyedEvents[ExternalOrderVanished](after = eventId)
          .map(_.event.externalOrderName)
          .toSet ==
          Set(
            ExternalOrderName(singletonFile.getFileName.toString),
            ExternalOrderName(newFile.getFileName.toString)))

        delete(oldFile)
      }

      delete(singletonFile)

      // Restore waitingFileWatch
      val eventId = eventWatch.lastAddedEventId
      controller.api.updateUnsignedSimpleItems(Seq(waitingFileWatch)).await(99.s).orThrow
      eventWatch.await[ItemAttached](_.event.key == waitingFileWatch.path, after = eventId)

    "Same filename both in old and new directory" in:
      TestJob.reset()
      TestJob.continue(1)

      // Same filename in old and new directory
      val name = "BOTH"
      val oldFile = waitingWatchDirectory / name
      val externalOrderName = ExternalOrderName(name)
      val orderId = waitingFileToOrderId(externalOrderName)
      oldFile := ""

      eventWatch.await[OrderFinished](_.key == orderId)
      assert(exists(oldFile))
      assert(eventWatch.eventsByKey[OrderFinished](orderId).last == OrderFinished())

      def orderWatchState = controllerState.keyTo(OrderWatchState)(waitingFileWatch.path)
      assert(orderWatchState == OrderWatchState(
        waitingFileWatch.withRevision(orderWatchState.item.itemRevision),
        externalToState = Map(
          externalOrderName -> HasOrder(orderId))))
      Logger.info(s"$orderWatchState")

      withTemporaryDirectory() { newDirectory =>
        val eventId = eventWatch.lastAddedEventId

        val newFile = newDirectory / name
        newFile := ""

        /// Change directory ///
        val changedFileWatch = waitingFileWatch.copy(
          directoryExpr = expr(StringConstant.quote(newDirectory.toString)))
        controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
        eventWatch.await[ItemAttached](after = eventId)

        /// Files are considered deleted (due to directory change — they still exists) ///
        eventWatch.awaitKey[OrderExternalVanished](orderId, after = eventId)
        eventWatch.awaitKey[OrderDeleted](orderId, after = eventId)

        // orderId has been started again because its filename duplicates in newDirectory

        eventWatch.awaitKey[ExternalOrderAppeared](waitingFileWatch.path, after = eventId)
        eventWatch.awaitKey[OrderAdded](orderId, after = eventId)

        TestJob.continue(1)

        eventWatch.awaitKey[OrderFinished](orderId, after = eventId)
        eventWatch.awaitKey[OrderDeleted](orderId, after = eventId)

        assert(eventWatch
          .keyedEvents[ExternalOrderAppeared](after = eventId)
          .map(_.event.arguments(FileWatch.FileArgumentName).asString.orThrow)
          .toSet == Set(newFile.toString))

        // Java for macOS needs 2s to detect a file (maybe oldFile)
        if isMac then sleep(2500.ms)

        assert(eventWatch
          .keyedEvents[ExternalOrderVanished](after = eventId)
          .map(_.event.externalOrderName)
          .toSet ==
          Set(
            ExternalOrderName(oldFile.getFileName.toString),
            ExternalOrderName(newFile.getFileName.toString)))

        eventWatch.await[OrderDeleted](_.key == orderId, after = eventId)
      }

      delete(oldFile)

      // Restore waitingFileWatch
      val eventId = eventWatch.lastAddedEventId
      controller.api.updateUnsignedSimpleItems(Seq(waitingFileWatch)).await(99.s).orThrow
      eventWatch.await[ItemAttached](_.event.key == waitingFileWatch.path, after = eventId)
    }

  "Change Agent" in:
    val eventId = eventWatch.lastAddedEventId
    val changedFileWatch = fileWatch.copy(agentPath = bAgentPath)
    controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
    eventWatch.await[ItemAttached](after = eventId)
    assert(eventWatch.keyedEvents[InventoryItemEvent](after = eventId) ==
      Seq(
        NoKey <-: UnsignedSimpleItemChanged(changedFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
        NoKey <-: ItemDetachable(fileWatch.path, aAgentPath),
        NoKey <-: ItemDetached(fileWatch.path, aAgentPath),
        NoKey <-: ItemAttachable(fileWatch.path, bAgentPath),
        NoKey <-: ItemAttached(fileWatch.path, Some(ItemRevision(1)), bAgentPath)))

  "JS-2159 Delete a FileWatch while an Order is still running" in:
    watchDirectory / "BEFORE-DELETION" := ""
    eventWatch.await[OrderFinished](_.key == OrderId("file:TEST-WATCH:BEFORE-DELETION"))

    watchDirectory / "BEFORE-DELETION" := ""
    eventWatch.await[OrderFinished](_.key == OrderId("file:TEST-WATCH:BEFORE-DELETION"))

  "Deleting the Workflow referenced by the FileWatch is rejected" in:
    assert(controller.api.updateItems(Stream(
      AddVersion(VersionId("TRY-DELETE")),
      RemoveVersioned(workflow.path)
    )).await(99.s) ==
      Left(ItemIsStillReferencedProblem(workflow.path, fileWatch.path,
        moreInfo = " with ExternalOrderName(BEFORE-DELETION), attached to Agent:AGENT-B")))

  "Delete a FileWatch" in:
    val eventId = eventWatch.lastAddedEventId
    assert(controller.api.updateItems(Stream(
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
    awaitAndAssert(controllerState.keyTo(OrderWatchState).isEmpty)


object FileWatchTest:
  private val logger = Logger[this.type]
  private val aAgentPath = AgentPath("AGENT-A")
  private val bAgentPath = AgentPath("AGENT-B")

  private val workflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Vector(DeleteFileJob.execute(aAgentPath)))

  private val waitingWorkflow = Workflow(
    WorkflowPath("WAITING-WORKFLOW"),
    Vector(
      TestJob.execute(aAgentPath)),
    orderPreparation = OrderPreparation(OrderParameterList(
      OrderParameter("file", StringValue),
      OrderParameter("DEFAULT", StringConstant("DEFAULT-VALUE")))))

  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
