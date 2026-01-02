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
import js7.data.order.Order.Finished
import js7.data.order.OrderEvent.{OrderAdded, OrderCancellationMarkedOnAgent, OrderDeleted, OrderExternalVanished, OrderFinished, OrderProcessingStarted}
import js7.data.order.OrderId
import js7.data.orderwatch.OrderWatchEvent.{ExternalOrderAppeared, ExternalOrderVanished}
import js7.data.orderwatch.OrderWatchState.HasOrder
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.plan.{PlanId, PlanSchemaId}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.{StringConstant, expr}
import js7.data.value.expression.scopes.EnvScope
import js7.data.value.expression.{Expression, ExpressionParser}
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
    ExpressionParser.expr(s"${StringConstant.quote(watchPrefix)} ++ env('$envName')"))

  private def externalToOrderId(externalOrderName: ExternalOrderName): OrderId =
    val (orderId, planId, priority) =
      fileWatch.externalToOrderAndPlanIdAndPriority(externalOrderName, None, Timestamp.now).orThrow
    assert(planId == PlanId.Global && priority == 0)
    orderId

  private def waitingFileToOrderId(externalOrderName: ExternalOrderName): OrderId =
    val (orderId, planId, priority) =
      waitingFileWatch.externalToOrderAndPlanIdAndPriority(externalOrderName, None, Timestamp.now).orThrow
    assert(planId == PlanId.Global && priority == 0)
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
    controller.await[ItemAttached](_.event.key == fileWatch.path)
    controller.await[ItemAttached](_.event.key == waitingFileWatch.path)

  "referencedItemPaths" in:
    assert(fileWatch.referencedItemPaths.toSet == Set(aAgentPath, workflow.path))

  "orderExpr" - {
    def evalOrderExpr(expr: Expression): (OrderId, PlanId, BigDecimal) =
      FileWatch(
        OrderWatchPath("ORDER-WATCH"),
        waitingWorkflow.path,
        aAgentPath,
        StringConstant("DIRECTORY"),
        orderExpr = Some(expr)
      ).externalToOrderAndPlanIdAndPriority(ExternalOrderName("1"), None, Timestamp.now)
        .orThrow

    "OrderId" in:
      val (orderId, planId, priority) = evalOrderExpr:
        expr"""{ orderId: "ORDER" }"""
      assert(orderId == OrderId("ORDER") && planId == PlanId.Global && priority == 0)

    "OrderId, PlanId" in:
      val (orderId, planId, priority) = evalOrderExpr:
        expr"""{ orderId: "ORDER", planId: [ "DailyPlan", "2025-07-01" ] }"""
      assert(orderId == OrderId("ORDER") && planId == PlanSchemaId("DailyPlan") / "2025-07-01" && priority == 0)

    "OrderId, PlanId, priority" in:
      val (orderId, planId, priority) = evalOrderExpr:
        expr"""{ orderId: "ORDER", planId: [ "DailyPlan", "2025-07-01" ], priority: 1}"""
      assert(orderId == OrderId("ORDER") && planId == PlanSchemaId("DailyPlan") / "2025-07-01" && priority == 1)
  }

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
      StringConstant(myDirectory.toString),
      orderExpr = Some(expr"""{ orderId: "EXISTING", priority: 7 }"""))
    updateItems(myFileWatch)

    val (orderId, planId, priority) =
      myFileWatch.externalToOrderAndPlanIdAndPriority(ExternalOrderName("1"), None, Timestamp.now).orThrow
    assert(planId == PlanId.Global && priority == 7)
    controller.awaitNextKey[OrderProcessingStarted](orderId)
    assert(controllerState.idToOrder(orderId).priority == 7)
    assert(controllerState.idToOrder(orderId).namedValues(waitingWorkflow).toMap == Map(
      "file" -> StringValue(file.toString),
      "DEFAULT" -> StringValue("DEFAULT-VALUE")))

    TestJob.continue()
    controller.awaitNextKey[OrderFinished](orderId)

    delete(file)
    controller.awaitNextKey[OrderDeleted](orderId)

    controller.api.updateItems(Stream(DeleteSimple(myFileWatch.path))).await(99.s).orThrow

  "Add a file" in:
    val file = watchDirectory / "2"
    val orderId = externalToOrderId(ExternalOrderName("2"))
    file := ""
    controller.awaitNextKey[OrderDeleted](orderId)
    assert(!exists(file))

  "Same filename again and again" in :
    for _ <- 1 to 3 do
      watchDirectory / "AGAIN" := ""
      controller.awaitNextKey[OrderFinished](OrderId("file:TEST-WATCH:AGAIN"))
      controller.awaitNextKey[OrderDeleted](OrderId("file:TEST-WATCH:AGAIN"))

  "Add many files, forcing an overflow" in:
    val since = Deadline.now
    val filenames = (1 to 1).map(_.toString).toVector
    val orderIds = filenames.map(ExternalOrderName(_)).map(externalToOrderId).toSet
    val whenAllRemoved = eventWatch
      .stream(EventRequest.singleClass[OrderDeleted](
        after = controller.lastAddedEventId,
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
    controller.awaitNextKey[OrderProcessingStarted](orderId)

    assert(controller.api.executeCommand(DeleteOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotDeleteWatchingOrderProblem(orderId)))

    TestJob.continue()
    controller.awaitNextKey[OrderFinished](orderId)
    intercept[TimeoutException]:
      controller.awaitNextKey[OrderDeleted](orderId, timeout = 100.ms)

    delete(file)
    val vanished = controller.awaitNextKey[ExternalOrderVanished](waitingFileWatch.path).head
    val removed = controller.awaitNextKey[OrderDeleted](orderId).head
    assert(vanished.timestamp <= removed.timestamp)

  "CancelOrder does not delete the order until the file has vanished" in:
    val file = waitingWatchDirectory / "CANCEL"
    val orderId = waitingFileToOrderId(ExternalOrderName("CANCEL"))
    file := ""
    controller.awaitNextKey[OrderProcessingStarted](orderId)

    execCmd(CancelOrders(orderId :: Nil))
    controller.awaitNextKey[OrderCancellationMarkedOnAgent](orderId)

    TestJob.continue()
    controller.awaitNextKey[OrderFinished](orderId)
    intercept[TimeoutException]:
      controller.awaitNextKey[OrderDeleted](orderId, timeout = 100.ms)

    assert(controller.api.executeCommand(DeleteOrdersWhenTerminated(orderId :: Nil)).await(99.s) ==
      Left(CannotDeleteWatchingOrderProblem(orderId)))

    delete(file)
    val vanished = controller.awaitNextKey[ExternalOrderVanished](waitingFileWatch.path).head
    val removed = controller.awaitNextKey[OrderDeleted](orderId).head
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
      val eventId = controller.resetLastWatchedEventId()
      val changedFileWatch = waitingFileWatch.copy(delay = i.ms/*little change*/)
      controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
      controller.awaitNext[ItemAttached](_.event.key == changedFileWatch.path)
      assert(controller.keyedEvents[InventoryItemEvent](after = eventId) ==
        Seq(
          NoKey <-: UnsignedSimpleItemChanged(changedFileWatch.copy(itemRevision = Some(itemRevision))),
          NoKey <-: ItemAttachable(changedFileWatch.path, aAgentPath),
          NoKey <-: ItemAttached(changedFileWatch.path, Some(itemRevision), aAgentPath)))

      val iFile = waitingWatchDirectory / s"AGAIN-$i"
      val iOrderId = waitingFileToOrderId(ExternalOrderName(s"AGAIN-$i"))
      iFile := ""
      TestJob.continue()
      controller.awaitNextKey[OrderFinished](iOrderId)

      delete(iFile)
      val vanished = controller.awaitNextKey[ExternalOrderVanished](waitingFileWatch.path).head
      val removed = controller.awaitNextKey[OrderDeleted](iOrderId).head
      assert(vanished.timestamp <= removed.timestamp)

    controller.awaitKey[OrderFinished](longOrderId)
    intercept[TimeoutException]:
      controller.awaitNextKey[OrderDeleted](longOrderId, timeout = 100.ms)
    delete(longFile)
    val vanished = controller.awaitNextKey[ExternalOrderVanished](waitingFileWatch.path).head
    val removed = controller.awaitNextKey[OrderDeleted](longOrderId).head
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

      controller.awaitNextKey[OrderFinished](singletonOrderId)
      assert(exists(singletonFile))

      withTemporaryDirectory() { newDirectory =>
        val eventId = controller.resetLastWatchedEventId()

        // A file only in the newDirectory
        val newFile = newDirectory / "CHANGE-DIRECTORY-NEW"
        val newOrderId = waitingFileToOrderId(ExternalOrderName("CHANGE-DIRECTORY-NEW"))
        newFile := ""

        /// Change directory ///
        val changedFileWatch = waitingFileWatch.copy(
          directoryExpr = ExpressionParser.expr(StringConstant.quote(newDirectory.toString)))
        controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
        controller.awaitNext[ItemAttachable](_.event.key == changedFileWatch.path)

        /// Files are considered deleted (due to directory change — they still exists) ///
        controller.awaitNextKey[ExternalOrderVanished](waitingFileWatch.path)
        controller.awaitNextKey[OrderExternalVanished](singletonOrderId)
        controller.awaitNextKey[OrderDeleted](singletonOrderId)

        // File in old directory is ignored
        val oldFile = waitingWatchDirectory / "CHANGE-DIRECTORY-OLD"
        oldFile := ""

        // bothOrderId has been started again because its filename duplicates in newDirectory

        controller.awaitNextKey[OrderFinished](newOrderId)
        assert(controllerState.idToOrder(newOrderId).state.isInstanceOf[Finished])

        delete(newFile)
        controller.awaitNextKey[ExternalOrderVanished](waitingFileWatch.path)
        controller.awaitNextKey[OrderExternalVanished](newOrderId)
        controller.awaitNextKey[OrderDeleted](newOrderId)

        assert(controller
          .keyedEvents[ExternalOrderAppeared](after = eventId)
          .map(_.event.arguments(FileWatch.FileArgumentName).asString.orThrow)
          .toSet ==
          Set(newFile.toString))

        // Java for macOS needs 2s to detect a file (maybe oldFile)
        awaitAndAssert(3.s)(controller
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
      controller.resetLastWatchedEventId()
      controller.api.updateUnsignedSimpleItems(Seq(waitingFileWatch)).await(99.s).orThrow
      controller.awaitNext[ItemAttached](_.event.key == waitingFileWatch.path)

    "Same filename both in old and new directory" in:
      TestJob.reset()
      TestJob.continue()

      // Same filename in old and new directory
      val name = "BOTH"
      val oldFile = waitingWatchDirectory / name
      val externalOrderName = ExternalOrderName(name)
      val orderId = waitingFileToOrderId(externalOrderName)
      oldFile := ""

      controller.awaitNextKey[OrderFinished](orderId)
      assert(exists(oldFile))
      assert(controllerState.idToOrder(orderId).state.isInstanceOf[Finished])

      def orderWatchState = controllerState.keyTo(OrderWatchState)(waitingFileWatch.path)
      assert(orderWatchState == OrderWatchState(
        waitingFileWatch.withRevision(orderWatchState.item.itemRevision),
        externalToState = Map(
          externalOrderName -> HasOrder(orderId))))
      Logger.info(s"$orderWatchState")

      withTemporaryDirectory() { newDirectory =>
        val eventId = controller.resetLastWatchedEventId()

        val newFile = newDirectory / name
        newFile := ""

        /// Change directory ///
        val changedFileWatch = waitingFileWatch.copy(
          directoryExpr = ExpressionParser.expr(StringConstant.quote(newDirectory.toString)))
        controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
        controller.awaitNext[ItemAttachable]()

        /// Files are considered deleted (due to directory change — they still exists) ///
        locally:
          val vanished = controller.awaitNextKey[OrderExternalVanished](orderId).head.eventId
          controller.awaitKey[OrderDeleted](orderId, after = vanished)
          // ExternalOrderAppeared comes concurrently with OrderDeleted
          controller.awaitKey[ExternalOrderAppeared](waitingFileWatch.path, after = vanished)

        // orderId has been started again because its filename duplicates in newDirectory
        controller.awaitNextKey[OrderAdded](orderId)

        TestJob.continue()

        controller.awaitNextKey[OrderFinished](orderId)

        assert(controller
          .keyedEvents[ExternalOrderAppeared](after = eventId)
          .map(_.event.arguments(FileWatch.FileArgumentName).asString.orThrow)
          .toSet == Set(newFile.toString))

        // Java for macOS needs 2s to detect a file (maybe oldFile)
        awaitAndAssert(3.s)(controller
          .keyedEvents[ExternalOrderVanished](after = eventId)
          .map(_.event.externalOrderName)
          .toSet ==
          Set(
            ExternalOrderName(oldFile.getFileName.toString),
            ExternalOrderName(newFile.getFileName.toString)))

        assert(controllerState.idToOrder(orderId).state.isInstanceOf[Finished])
        delete(newFile)
        controller.awaitNextKey[OrderDeleted](orderId)
      }

      delete(oldFile)

      // Restore waitingFileWatch
      val eventId = controller.resetLastWatchedEventId()
      controller.api.updateUnsignedSimpleItems(Seq(waitingFileWatch)).await(99.s).orThrow
      controller.awaitNext[ItemAttached](_.event.key == waitingFileWatch.path)
    }

  "Change Agent" in:
    val eventId = controller.resetLastWatchedEventId()
    val changedFileWatch = fileWatch.copy(agentPath = bAgentPath)
    controller.api.updateUnsignedSimpleItems(Seq(changedFileWatch)).await(99.s).orThrow
    controller.awaitNext[ItemAttached]()
    assert(controller.keyedEvents[InventoryItemEvent](after = eventId) ==
      Seq(
        NoKey <-: UnsignedSimpleItemChanged(changedFileWatch.copy(itemRevision = Some(ItemRevision(1)))),
        NoKey <-: ItemDetachable(fileWatch.path, aAgentPath),
        NoKey <-: ItemDetached(fileWatch.path, aAgentPath),
        NoKey <-: ItemAttachable(fileWatch.path, bAgentPath),
        NoKey <-: ItemAttached(fileWatch.path, Some(ItemRevision(1)), bAgentPath)))

  "JS-2159 Delete a FileWatch while an Order is still running" in:
    waitingWatchDirectory / "DELETE-WATCH" := ""
    val orderId = OrderId("file:WAITING-WATCH:DELETE-WATCH")
    controller.awaitNextKey[OrderProcessingStarted](orderId)

    controller.api.updateItems:
      Stream:
        DeleteSimple(waitingFileWatch.path)
    .await(99.s).orThrow
    controller.awaitNextKey[ExternalOrderVanished](waitingFileWatch.path)
    controller.awaitNext[ItemDeleted](_.event.key == waitingFileWatch.path)

    TestJob.continue()
    controller.awaitNextKey[OrderFinished](orderId)
    controller.awaitNextKey[OrderDeleted](orderId)

    delete(waitingWatchDirectory / "DELETE-WATCH")

  "Deleting the Workflow referenced by the FileWatch is rejected" in:
    assert:
      controller.api.updateItems:
        Stream(
          AddVersion(VersionId("TRY-DELETE")),
          RemoveVersioned(workflow.path))
      .await(99.s) ==
        Left(ItemIsStillReferencedProblem(workflow.path, fileWatch.path,
          moreInfo = ", attached to Agent:AGENT-B"))

  "Delete a FileWatch" in:
    val eventId = controller.resetLastWatchedEventId()
    assert:
      controller.api.updateItems:
        Stream(
          DeleteSimple(fileWatch.path),
          DeleteSimple(waitingFileWatch.path))
      .await(99.s) == Right(Completed)
    controller.awaitNext[ItemDeleted](_.event.key == fileWatch.path)
    val events = controller.keyedEvents[InventoryItemEvent](after = eventId)
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
    Vector(
      DeleteFileJob.execute(aAgentPath)))

  private val waitingWorkflow = Workflow(
    WorkflowPath("WAITING-WORKFLOW"),
    Vector(
      TestJob.execute(aAgentPath)),
    orderPreparation = OrderPreparation(OrderParameterList(
      OrderParameter("file", StringValue),
      OrderParameter("DEFAULT", StringConstant("DEFAULT-VALUE")))))

  private class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
