package js7.tests.order

import cats.effect.IO
import cats.effect.std.Semaphore
import cats.effect.unsafe.IORuntime
import java.nio.file.Files.{createTempFile, deleteIfExists}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.touchFile
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.{CorrelId, CorrelIdWrapped, Logger}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerCommand.{AddOrder, AnswerOrderPrompt, Batch, CancelOrders, Response, ResumeOrder, ResumeOrders, SuspendOrders}
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, InsertHistoricOutcome, ReplaceHistoricOutcome}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarkedOnAgent, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderResumed, OrderResumptionMarked, OrderRetrying, OrderStarted, OrderStdWritten, OrderStdoutWritten, OrderSuspended, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent, OrderTerminated}
import js7.data.order.{FreshOrder, HistoricOutcome, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{EmptyInstruction, Execute, Fail, Fork, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.BranchId.{Try_, catch_, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.EmptyJob
import js7.tests.order.SuspendResumeOrdersTest.*
import js7.tests.testenv.DirectoryProvider.{toLocalSubagentId, waitingForFileScript}
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}

final class SuspendResumeOrdersTest
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(singleJobWorkflow, twoJobsWorkflow, forkWorkflow, tryWorkflow, failingWorkflow)

  private lazy val triggerFile = createTempFile("SuspendResumeOrdersTest-", ".tmp")

  import controller.api.{addOrder, executeCommand}

  override def beforeAll() =
    for a <- directoryProvider.agentEnvs do
      a.writeExecutable(pathExecutable, waitingForFileScript(triggerFile, delete = true))
    super.beforeAll()
    controller.eventWatch.await[AgentReady]()

  override def afterAll() =
    try
      deleteIfExists(triggerFile)
    finally
      super.afterAll()

  "Suspend and resume a fresh order" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔺"), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 2.s/*1s too short in rare cases*/))
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspended](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id) == Seq(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderSuspensionMarked(),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))
    val lastEventId = eventWatch.lastAddedEventId

    touchFile(triggerFile)
    executeCommand(ResumeOrders(Set(order.id))).await(99.s).orThrow

    // TODO Modify order start time here, when possible. Otherwise we wait until the scheduled start time

    // ResumeOrders command expected a suspended or suspending order
    assert(executeCommand(ResumeOrders(Set(order.id))).await(99.s) == Left(CannotResumeOrderProblem))

    eventWatch.await[OrderFinished](_.key == order.id)
    assert(eventWatch.eventsByKey[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "An order reaching end of workflow is suspendible" in:
    val order = FreshOrder(OrderId("🔻"), singleJobWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)
    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id)
    assert(eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(),
      OrderSuspensionMarkedOnAgent,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = eventWatch.lastAddedEventId
    executeCommand(ResumeOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(),
      OrderFinished()))

  "Suspend with kill" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("♣️"), singleJobWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    sleep(200.ms) // Wait until process has been started

    executeCommand(SuspendOrders(Set(order.id), SuspensionMode(Some(CancellationMode.Kill()))))
      .await(99.s).orThrow
    eventWatch.await[OrderSuspended](_.key == order.id)

    val events = eventWatch.eventsByKey[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten])
      .map:
        case OrderProcessed(OrderOutcome.Killed(failed: OrderOutcome.Failed)) if failed.namedValues == NamedValues.rc(SIGKILL) =>
          // Sometimes, SIGTERM does not work and SIGKILL be sent. Something wrong with the bash script ????
          logger.error("SIGTERM did not work")
          OrderProcessed(OrderOutcome.Killed(failed.copy(namedValues = NamedValues.rc(SIGTERM))))  // Repair, to let test succceed
        case o => o

    assert(events == Seq(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(SuspensionMode(Some(CancellationMode.Kill()))),
      OrderSuspensionMarkedOnAgent,
      OrderProcessed(OrderOutcome.Killed(
        if isWindows then
          OrderOutcome.Failed.rc(1)
        else
          OrderOutcome.Failed(NamedValues.rc(SIGTERM)))),
      OrderProcessingKilled,
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = eventWatch.lastAddedEventId
    touchFile(triggerFile)
    executeCommand(ResumeOrders(Set(order.id), asSucceeded = true)).await(99.s).orThrow
    eventWatch.await[OrderTerminated](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed(asSucceeded = true),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "Suspend and resume orders between two jobs" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("♦️"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow

    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    executeCommand(SuspendOrders(Seq(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)
    touchFile(triggerFile)

    eventWatch.await[OrderSuspended](_.key == order.id)
      assert(eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderSuspensionMarked(),
        OrderSuspensionMarkedOnAgent,
        OrderProcessed(OrderOutcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderSuspended))

    val lastEventId = eventWatch.lastAddedEventId
    touchFile(triggerFile)
    executeCommand(ResumeOrders(Seq(order.id))).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == order.id)
    assert(eventWatch.eventsByKey[OrderEvent](order.id, after = lastEventId)
      .filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderResumed(),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeededRC0),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderFinished()))

  "An order being cancelled is not suspendible nor resumable" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔷"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())).await(99.s).orThrow
    assert(executeCommand(SuspendOrders(Set(order.id))).await(99.s) == Left(CannotSuspendOrderProblem))
    eventWatch.await[OrderCancellationMarkedOnAgent](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderCancelled](_.key == order.id)

  "Suspending a forked order does not suspend child orders" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    touchFile(triggerFile)
    eventWatch.await[OrderProcessed](_.key == order.id / "🥕")

    //eventWatch.await[OrderProcessingStarted](_.key == (order.id | "🥕"))
    //touchFile(bTriggerFile)
    //waitForCondition(10.s, 10.ms) { !exists(triggerFile) }
    //assert(!exists(triggerFile))
    eventWatch.await[OrderJoined](_.key == order.id)
    eventWatch.await[OrderSuspended](_.key == order.id)

    assert(eventWatch
      .allKeyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Seq(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.arguments, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Vector("🥕" -> OrderId("FORK|🥕"))),
        OrderId("FORK|🥕") <-: OrderAttachable(agentPath),
        OrderId("FORK|🥕") <-: OrderAttached(agentPath),
        OrderId("FORK|🥕") <-: OrderProcessingStarted(subagentId),
        OrderId("FORK") <-: OrderSuspensionMarked(),
        OrderId("FORK|🥕") <-: OrderProcessed(OrderOutcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK|🥕") <-: OrderDetachable,
        OrderId("FORK|🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(OrderOutcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderSuspended))

  "Suspend unknown order" in:
    assert(executeCommand(SuspendOrders(Set(OrderId("UNKNOWN")))).await(99.s) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))

  "Suspend multiple orders with Batch" in:
    deleteIfExists(triggerFile)
    val orders = for i <- 1 to 3 yield
      FreshOrder(OrderId(i.toString), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.s))
    for o <- orders do addOrder(o).await(99.s).orThrow
    for o <- orders do eventWatch.await[OrderAttached](_.key == o.id)
    val response = executeCommand(Batch(
      for o <- orders yield CorrelIdWrapped(CorrelId.empty, SuspendOrders(Set(o.id))))
    ).await(99.s).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for o <- orders do eventWatch.await[OrderSuspended](_.key == o.id)

  "Resume a still suspending order" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔹"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)

    executeCommand(ResumeOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderResumptionMarked](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderProcessed](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderFinished](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(),
      OrderSuspensionMarkedOnAgent,
      OrderResumptionMarked(),
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),

      // AgentOrderKeeper does not properly handle simulataneous ExecuteMarkOrder commands
      // and so order is detached for suspending (which has been withdrawn by ResumeOrders).
      OrderDetachable,
      OrderDetached,
      OrderAttachable(agentPath),
      OrderAttached(agentPath),

      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))

  "Resume with position a still suspending order is inhibited" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🟦"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)

    assert(executeCommand(ResumeOrder(order.id, Some(Position(0)))).await(99.s) ==
      Left(CannotResumeOrderProblem))

    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id)
    assert(eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(),
      OrderSuspensionMarkedOnAgent,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    executeCommand(CancelOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)

  "Suspend and resume twice on same Agent" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🟧"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id)
    val eventId = eventWatch.lastAddedEventId

    executeCommand(ResumeOrder(order.id)).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id, after = eventId)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id, after = eventId)

    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id, after = eventId)
    executeCommand(ResumeOrder(order.id)).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == order.id, after = eventId)

    assert(eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(),
      OrderSuspensionMarkedOnAgent,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended,

      OrderResumed(),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(),
      OrderSuspensionMarkedOnAgent,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended,

      OrderResumed(),
      OrderFinished()))

  "Resume with invalid position is rejected" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("INVALID-POSITION"), tryWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id)
    assert(
      executeCommand(ResumeOrder(order.id, Some(Position(99)))).await(99.s) ==
        Left(UnreachableOrderPositionProblem))

    val invalidOps = Seq(
      ReplaceHistoricOutcome(Position(99), OrderOutcome.succeeded),
      InsertHistoricOutcome(Position(99), Position(0), OrderOutcome.succeeded),
      InsertHistoricOutcome(Position(0), Position(99), OrderOutcome.succeeded),
      DeleteHistoricOutcome(Position(99)),
      AppendHistoricOutcome(Position(99), OrderOutcome.succeeded))
    for op <- invalidOps do assert(
      executeCommand(ResumeOrder(order.id, historyOperations = Seq(op))).await(99.s) ==
        Left(Problem("Unknown position 99 in Workflow:TRY~INITIAL")))

    executeCommand(CancelOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)

  "Resume with changed position and changed historic outcomes" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔶"), tryWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)
    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(tryWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(),
      OrderSuspensionMarkedOnAgent,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = eventWatch.lastAddedEventId
    val newPosition = Position(2) / Try_ % 0
    val historicOutcomeOps = Seq(
      ReplaceHistoricOutcome(Position(0), OrderOutcome.Succeeded(Map("NEW" -> NumberValue(1)))),
      AppendHistoricOutcome(Position(1), OrderOutcome.Succeeded(Map("NEW" -> NumberValue(2)))))

    executeCommand(ResumeOrder(order.id, Some(newPosition), historicOutcomeOps))
      .await(99.s).orThrow
    eventWatch.await[OrderFailed](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id, after = lastEventId)
      .filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderResumed(Some(newPosition), historicOutcomeOps),
        OrderOutcomeAdded(OrderOutcome.Failed(Some("FAILURE"))),
        OrderCaught(Position(2) / catch_(0) % 0),
        OrderRetrying(Position(2) / try_(1) % 0, None),
        OrderOutcomeAdded(OrderOutcome.Failed(Some("FAILURE"))),
        OrderFailed(Position(2) / try_(1) % 0)))

    assert(controller.orderApi.order(order.id).await(99.s) == Right(Some(Order(
      order.id, order.workflowPath ~ "INITIAL" /: (Position(2) / try_(1) % 0),
      Order.Failed,
      historicOutcomes = Vector(
        HistoricOutcome(Position(0), OrderOutcome.Succeeded(Map("NEW" -> NumberValue(1)))),
        HistoricOutcome(Position(1), OrderOutcome.Succeeded(Map("NEW" -> NumberValue(2)))),
        HistoricOutcome(Position(2) / Try_ % 0, OrderOutcome.Failed(Some("FAILURE"))),
        HistoricOutcome(Position(2) / catch_(0) % 0, OrderOutcome.succeeded),
        HistoricOutcome(Position(2) / try_(1) % 0, OrderOutcome.Failed(Some("FAILURE"))))))))

  "Resume when Failed" in:
    val order = FreshOrder(OrderId("🟫"), failingWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderFailed](_.key == order.id)

    var eventId = eventWatch.lastAddedEventId
    assert(executeCommand(SuspendOrders(Seq(order.id))).await(99.s) == Left(CannotSuspendOrderProblem))
    executeCommand(ResumeOrder(order.id, asSucceeded = true)).await(99.s).orThrow
    eventWatch.await[OrderFailed](_.key == order.id, after = eventId)
    assert(controllerState.idToOrder(order.id).historicOutcomes == Seq(
      HistoricOutcome(Position(0), OrderOutcome.succeeded),
      HistoricOutcome(Position(1), OrderOutcome.failed),
      HistoricOutcome(Position(1), OrderOutcome.succeeded)/*Resume asSucceeded*/,
      HistoricOutcome(Position(1), OrderOutcome.failed)))

    eventId = eventWatch.lastAddedEventId
    executeCommand(ResumeOrder(order.id, Some(Position(0)), asSucceeded = true)).await(99.s).orThrow

    eventWatch.await[OrderFailed](_.key == order.id, after = eventId)
    assert(controllerState.idToOrder(order.id).historicOutcomes == Seq(
      HistoricOutcome(Position(1), OrderOutcome.succeeded)/*Resume asSucceeded*/,
      HistoricOutcome(Position(0), OrderOutcome.succeeded),
      HistoricOutcome(Position(1), OrderOutcome.failed)))

    assert(eventWatch.eventsByKey[OrderEvent](order.id) == Seq(
      OrderAdded(failingWorkflow.id, order.arguments, order.scheduledFor),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1)),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderDetachable,
      OrderDetached,

      OrderFailed(Position(1)),

      OrderResumed(asSucceeded = true),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderFailed(Position(1)),

      OrderResumed(Some(Position(0)), asSucceeded = true),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1)),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderDetachable,
      OrderDetached,
      OrderFailed(Position(1))))

  "Suspend and resume at end of workflow" in:
    val workflow = Workflow(
      WorkflowPath("SUSPEND-AT-END"),
      Seq(
        Prompt(expr("'PROMPT'"))))
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("SUSPEND-AT-END")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.await[OrderPrompted](_.key == orderId)

      controller.api.executeCommand(SuspendOrders(Seq(orderId))).await(99.s).orThrow
      controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
      eventWatch.await[OrderSuspended](_.key == orderId)

      controller.api.executeCommand(ResumeOrders(Seq(orderId))).await(99.s).orThrow
      eventWatch.await[OrderDeleted](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderPrompted(StringValue("PROMPT")),
        OrderSuspensionMarked(SuspensionMode(None)),
        OrderPromptAnswered(),
        OrderMoved(Position(1)),
        OrderSuspended,
        OrderResumed(),
        OrderFinished(),
        OrderDeleted))
    }

  "Suspend then cancel" - {
    "Suspend in the middle of a workflow, then cancel" in:
      testSuspendAndCancel(
        Workflow(
          WorkflowPath("SUSPEND-THEN-CANCEL"),
          Seq(
            Prompt(expr("'PROMPT'")),
            EmptyInstruction())))

    "Suspend at end of workflow, then cancel" in:
      testSuspendAndCancel(
        Workflow(
          WorkflowPath("SUSPEND-AT-END-THEN-CANCEL"),
          Seq(
            Prompt(expr("'PROMPT'")))))

    def testSuspendAndCancel(workflow: Workflow): Unit =
      withTemporaryItem(workflow) { workflow =>
        val orderId = OrderId(workflow.path.string)
        controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        eventWatch.await[OrderPrompted](_.key == orderId)

        controller.api.executeCommand(SuspendOrders(Seq(orderId))).await(99.s).orThrow
        controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
        eventWatch.await[OrderSuspended](_.key == orderId)

        controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
        eventWatch.await[OrderDeleted](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderPrompted(StringValue("PROMPT")),
          OrderSuspensionMarked(SuspensionMode(None)),
          OrderPromptAnswered(),
          OrderMoved(Position(1)),
          OrderSuspended,
          OrderCancelled,
          OrderDeleted))
      }
  }

  // Test moved to RetryTest:
  // "FIX JS-2089 Cancel an Order waiting in Retry instruction at an Agent"

  "Suspend a processing Order that will fail" in:
    // The failed order will not be suspended, because it failed.
    val workflow = Workflow.of(WorkflowPath("FAIL"),
      FailingSemaJob.execute(agentPath),
      EmptyInstruction())
    withTemporaryItem(workflow): workflow =>
      val orderId = OrderId("SUSPEND-FAILING-JOB")
      var eventId = eventWatch.lastAddedEventId
      controller.api
        .executeCommand(AddOrder(
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)))
        .await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

      controller.api.executeCommand(SuspendOrders(Seq(orderId))).await(99.s).orThrow
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == orderId, after = eventId)

      FailingSemaJob.semaphore.flatMap(_.release).await(99.s)
      eventWatch.await[OrderFailed](_.key == orderId, after = eventId)

      // OrderFailed event has reset OrderMark.Suspending
      assert(controllerState.idToOrder(orderId).mark == None)

      eventId = eventWatch.lastAddedEventId
      controller.api
        .executeCommand(
          ResumeOrder(orderId, Some(Position(1)), asSucceeded = true))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("FailingJob\n"),
        OrderSuspensionMarked(),
        OrderSuspensionMarkedOnAgent,
        OrderProcessed(OrderOutcome.failed),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0)),
        OrderResumed(Some(Position(1)), asSucceeded = true),
        OrderMoved(Position(2)),
        OrderFinished(),
        OrderDeleted))

  "Suspend a forked processing Order that will fail" in:
    val workflow = Workflow.of(WorkflowPath("FAIL-IN-FORK"),
      Fork(
        Vector(
          "BRANCH" -> Workflow.of(
            FailingSemaJob.execute(agentPath),
            EmptyInstruction())),
        joinIfFailed = false))

    withTemporaryItem(workflow): workflow =>
      val orderId = OrderId("SUSPEND-FORKED-FAILING-JOB")
      val childOrderId = OrderId("SUSPEND-FORKED-FAILING-JOB|BRANCH")
      var eventId = eventWatch.lastAddedEventId
      controller.api
        .executeCommand(AddOrder(
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)))
        .await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == childOrderId, after = eventId)

      controller.api.executeCommand(SuspendOrders(Seq(childOrderId))).await(99.s).orThrow
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == childOrderId, after = eventId)

      FailingSemaJob.semaphore.flatMap(_.release).await(99.s)
      eventWatch.await[OrderFailed](_.key == childOrderId, after = eventId)

      // OrderFailed event has reset OrderMark.Suspending
      assert(controllerState.idToOrder(childOrderId).mark == None)

      eventId = eventWatch.lastAddedEventId
      controller.api
        .executeCommand(
          ResumeOrder(childOrderId, Some(Position(0) / BranchId.fork("BRANCH") % 1),
            asSucceeded = true))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderForked(Vector(
          "BRANCH" -> childOrderId)),
        OrderJoined(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

      assert(eventWatch.eventsByKey[OrderEvent](childOrderId) == Seq(
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("FailingJob\n"),
        OrderSuspensionMarked(),
        OrderSuspensionMarkedOnAgent,
        OrderProcessed(OrderOutcome.failed),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0) / BranchId.fork("BRANCH") % 0),
        OrderResumed(Some(Position(0) / BranchId.fork("BRANCH") % 1), asSucceeded = true),
        OrderMoved(Position(0) / BranchId.fork("BRANCH") % 2)))

  "Suspend a forked processing Order that will fail, joinIfFailed = true" in:
    val workflow = Workflow.of(WorkflowPath("FAIL-IN-FORK-JOIN-IF-FAILED"),
      Fork(
        Vector(
          "BRANCH" -> Workflow.of(
            FailingSemaJob.execute(agentPath),
            EmptyInstruction())),
        joinIfFailed = true))

    withTemporaryItem(workflow): workflow =>
      val orderId = OrderId("SUSPEND-FORKED-FAILING-JOB-JOIN-IF-FAILED")
      val childOrderId = OrderId("SUSPEND-FORKED-FAILING-JOB-JOIN-IF-FAILED|BRANCH")
      var eventId = eventWatch.lastAddedEventId
      controller.api
        .executeCommand(AddOrder(
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)))
        .await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == childOrderId, after = eventId)

      controller.api.executeCommand(SuspendOrders(Seq(childOrderId))).await(99.s).orThrow
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == childOrderId, after = eventId)

      FailingSemaJob.semaphore.flatMap(_.release).await(99.s)
      eventWatch.await[OrderFailedInFork](_.key == childOrderId, after = eventId)

      if true then
        eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
        pending // TODO Allow to suspend a failing child order before join?
      else
        // OrderFailed event has reset OrderMark.Suspending
        assert(controllerState.idToOrder(childOrderId).mark == None)

        eventId = eventWatch.lastAddedEventId
          controller.api
            .executeCommand(
              ResumeOrder(childOrderId, Some(Position(0) / BranchId.fork("BRANCH") % 1),
                asSucceeded = true))
            .await(99.s).orThrow
        eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderForked(Vector(
            "BRANCH" -> childOrderId)),
          OrderJoined(OrderOutcome.succeeded),
          OrderMoved(Position(1)),
          OrderFinished(),
          OrderDeleted))

        assert(eventWatch.eventsByKey[OrderEvent](childOrderId) == Seq(
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("FailingJob\n"),
          OrderSuspensionMarked(),
          OrderSuspensionMarkedOnAgent,
          OrderProcessed(OrderOutcome.failed),
          OrderDetachable,
          OrderDetached,
          OrderFailedInFork(Position(0) / BranchId.fork("BRANCH") % 0),
          OrderResumed(Some(Position(0) / BranchId.fork("BRANCH") % 1), asSucceeded = true),
          OrderMoved(Position(0) / BranchId.fork("BRANCH") % 2)))


object SuspendResumeOrdersTest:

  private val logger = Logger[this.type]
  private val pathExecutable = RelativePathExecutable("executable.cmd")
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val versionId = VersionId("INITIAL")
  private val executeJob = Execute(WorkflowJob(agentPath, pathExecutable, processLimit = 100))

  private val singleJobWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    executeJob)

  private val twoJobsWorkflow = Workflow.of(
    WorkflowPath("TWO") ~ versionId,
    executeJob,
    executeJob)

  private val forkWorkflow = Workflow.of(
    WorkflowPath("FORK") ~ versionId,
    Fork.of(
      "🥕" -> Workflow.of(
      executeJob)),
    executeJob)

  private val tryWorkflow = Workflow.of(
    WorkflowPath("TRY") ~ versionId,
    executeJob,
    executeJob,
    TryInstruction(
      Workflow.of(
        Fail(Some(expr("'FAILURE'")))),
      Workflow.of(
        Retry()),
      maxTries = Some(2)))

  private val failingWorkflow = Workflow.of(
    WorkflowPath("FAILING") ~ versionId,
    EmptyJob.execute(agentPath),
    Fail())

  final class FailingSemaJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess:
        step.writeOut("FailingJob\n")
          .*>(FailingSemaJob.semaphore)
          .flatMap(_.acquire)
          .as(OrderOutcome.failed)

  private object FailingSemaJob extends InternalJob.Companion[FailingSemaJob]:
    val semaphore = Semaphore[IO](0).unsafeMemoize
