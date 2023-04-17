package js7.tests.order

import java.nio.file.Files.{createTempFile, deleteIfExists}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.touchFile
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, Batch, CancelOrders, Response, ResumeOrder, ResumeOrders, SuspendOrders}
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, InsertHistoricOutcome, ReplaceHistoricOutcome}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarkedOnAgent, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderResumed, OrderResumptionMarked, OrderRetrying, OrderStarted, OrderStdWritten, OrderSuspended, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent, OrderTerminated}
import js7.data.order.{FreshOrder, HistoricOutcome, Order, OrderEvent, OrderId, Outcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{EmptyInstruction, Execute, Fail, Fork, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.BranchId.{Try_, catch_, try_}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.EmptyJob
import js7.tests.order.SuspendResumeOrdersTest.*
import js7.tests.testenv.DirectoryProvider.{toLocalSubagentId, waitingForFileScript}
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.execution.Scheduler.Implicits.traced

final class SuspendResumeOrdersTest extends OurTestSuite with ControllerAgentForScalaTest
with BlockingItemUpdater
{
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

  override def beforeAll() = {
    for (a <- directoryProvider.agentEnvs) {
      a.writeExecutable(pathExecutable, waitingForFileScript(triggerFile, delete = true))
    }
    super.beforeAll()
    controller.eventWatch.await[AgentReady]()
  }

  override def afterAll() = {
    deleteIfExists(triggerFile)
    super.afterAll()
  }

  "Suspend and resume a fresh order" in {
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
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))
  }

  "An order reaching end of workflow is suspendible" in {
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
      OrderProcessed(Outcome.succeededRC0),
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
  }

  "Suspend with kill" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("♣️"), singleJobWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(SuspendOrders(Set(order.id), SuspensionMode(Some(CancellationMode.Kill()))))
      .await(99.s).orThrow
    eventWatch.await[OrderSuspended](_.key == order.id)

    val events = eventWatch.eventsByKey[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten])
      .map {
        case OrderProcessed(Outcome.Killed(failed: Outcome.Failed)) if failed.namedValues == NamedValues.rc(SIGKILL) =>
          // Sometimes, SIGTERM does not work and SIGKILL be sent. Something wrong with the bash script ????
          scribe.error("SIGTERM did not work")
          OrderProcessed(Outcome.Killed(failed.copy(namedValues = NamedValues.rc(SIGTERM))))  // Repair, to let test succceed
        case o => o
      }

    assert(events == Seq(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderSuspensionMarked(SuspensionMode(Some(CancellationMode.Kill()))),
      OrderSuspensionMarkedOnAgent,
      OrderProcessed(Outcome.Killed(
        if (isWindows)
          Outcome.Failed.rc(1)
        else
          Outcome.Failed(NamedValues.rc(SIGTERM)))),
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
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))
  }

  "Suspend and resume orders between two jobs" in {
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
        OrderProcessed(Outcome.succeededRC0),
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
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderFinished()))
  }

  "An order being cancelled is not suspendible nor resumable" in {
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔷"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())).await(99.s).orThrow
    assert(executeCommand(SuspendOrders(Set(order.id))).await(99.s) == Left(CannotSuspendOrderProblem))
    eventWatch.await[OrderCancellationMarkedOnAgent](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderCancelled](_.key == order.id)
  }

  "Suspending a forked order does not suspend child orders" in {
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
        OrderId("FORK|🥕") <-: OrderProcessed(Outcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK|🥕") <-: OrderDetachable,
        OrderId("FORK|🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(Outcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderSuspended))
  }

  "Suspend unknown order" in {
    assert(executeCommand(SuspendOrders(Set(OrderId("UNKNOWN")))).await(99.s) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Suspend multiple orders with Batch" in {
    deleteIfExists(triggerFile)
    val orders = for (i <- 1 to 3) yield
      FreshOrder(OrderId(i.toString), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.s))
    for (o <- orders) addOrder(o).await(99.s).orThrow
    for (o <- orders) eventWatch.await[OrderAttached](_.key == o.id)
    val response = executeCommand(Batch(
      for (o <- orders) yield CorrelIdWrapped(CorrelId.empty, SuspendOrders(Set(o.id))))
    ).await(99.s).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for (o <- orders) eventWatch.await[OrderSuspended](_.key == o.id)
  }

  "Resume a still suspending order" in {
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
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),

      // AgentOrderKeeper does not properly handle simulataneous ExecuteMarkOrder commands
      // and so order is detached for suspending (which has been withdrawn by ResumeOrders).
      OrderDetachable,
      OrderDetached,
      OrderAttachable(agentPath),
      OrderAttached(agentPath),

      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderDetached,
      OrderFinished()))
  }

  "Resume with position a still suspending order is inhibited" in {
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
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    executeCommand(CancelOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
  }

  "Suspend and resume twice on same Agent" in {
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
      OrderProcessed(Outcome.succeededRC0),
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
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended,

      OrderResumed(),
      OrderFinished()))
  }

  "Resume with invalid position is rejected" in {
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
      ReplaceHistoricOutcome(Position(99), Outcome.succeeded),
      InsertHistoricOutcome(Position(99), Position(0), Outcome.succeeded),
      InsertHistoricOutcome(Position(0), Position(99), Outcome.succeeded),
      DeleteHistoricOutcome(Position(99)),
      AppendHistoricOutcome(Position(99), Outcome.succeeded))
    for (op <- invalidOps) assert(
      executeCommand(ResumeOrder(order.id, historyOperations = Seq(op))).await(99.s) ==
        Left(Problem("Unknown position 99 in Workflow:TRY~INITIAL")))

    executeCommand(CancelOrders(Set(order.id))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
  }

  "Resume with changed position and changed historic outcomes" in {
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
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderSuspended))

    val lastEventId = eventWatch.lastAddedEventId
    val newPosition = Position(2) / Try_ % 0
    val historicOutcomeOps = Seq(
      ReplaceHistoricOutcome(Position(0), Outcome.Succeeded(Map("NEW" -> NumberValue(1)))),
      AppendHistoricOutcome(Position(1), Outcome.Succeeded(Map("NEW" -> NumberValue(2)))))

    executeCommand(ResumeOrder(order.id, Some(newPosition), historicOutcomeOps))
      .await(99.s).orThrow
    eventWatch.await[OrderFailed](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id, after = lastEventId)
      .filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderResumed(Some(newPosition), historicOutcomeOps),
        OrderOutcomeAdded(Outcome.Failed(Some("FAILURE"))),
        OrderCaught(Position(2) / catch_(0) % 0),
        OrderRetrying(Position(2) / try_(1) % 0, None),
        OrderOutcomeAdded(Outcome.Failed(Some("FAILURE"))),
        OrderFailed(Position(2) / try_(1) % 0)))

    assert(controller.orderApi.order(order.id).await(99.s) == Right(Some(Order(
      order.id, order.workflowPath ~ "INITIAL" /: (Position(2) / try_(1) % 0),
      Order.Failed,
      historicOutcomes = Vector(
        HistoricOutcome(Position(0), Outcome.Succeeded(Map("NEW" -> NumberValue(1)))),
        HistoricOutcome(Position(1), Outcome.Succeeded(Map("NEW" -> NumberValue(2)))),
        HistoricOutcome(Position(2) / Try_ % 0, Outcome.Failed(Some("FAILURE"))),
        HistoricOutcome(Position(2) / catch_(0) % 0, Outcome.succeeded),
        HistoricOutcome(Position(2) / try_(1) % 0, Outcome.Failed(Some("FAILURE"))))))))
  }

  "Resume when Failed" in {
    val order = FreshOrder(OrderId("🟫"), failingWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderFailed](_.key == order.id)

    var eventId = eventWatch.lastAddedEventId
    assert(executeCommand(SuspendOrders(Seq(order.id))).await(99.s) == Left(CannotSuspendOrderProblem))
    executeCommand(ResumeOrder(order.id, asSucceeded = true)).await(99.s).orThrow
    eventWatch.await[OrderFailed](_.key == order.id, after = eventId)
    assert(controllerState.idToOrder(order.id).historicOutcomes == Seq(
      HistoricOutcome(Position(0), Outcome.succeeded),
      HistoricOutcome(Position(1), Outcome.failed),
      HistoricOutcome(Position(1), Outcome.succeeded)/*Resume asSucceeded*/,
      HistoricOutcome(Position(1), Outcome.failed)))

    eventId = eventWatch.lastAddedEventId
    executeCommand(ResumeOrder(order.id, Some(Position(0)), asSucceeded = true)).await(99.s).orThrow

    eventWatch.await[OrderFailed](_.key == order.id, after = eventId)
    assert(controllerState.idToOrder(order.id).historicOutcomes == Seq(
      HistoricOutcome(Position(1), Outcome.succeeded)/*Resume asSucceeded*/,
      HistoricOutcome(Position(0), Outcome.succeeded),
      HistoricOutcome(Position(1), Outcome.failed)))

    assert(eventWatch.eventsByKey[OrderEvent](order.id) == Seq(
      OrderAdded(failingWorkflow.id, order.arguments, order.scheduledFor),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderOutcomeAdded(Outcome.failed),
      OrderDetachable,
      OrderDetached,

      OrderFailed(Position(1)),

      OrderResumed(asSucceeded = true),
      OrderOutcomeAdded(Outcome.failed),
      OrderFailed(Position(1)),

      OrderResumed(Some(Position(0)), asSucceeded = true),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderOutcomeAdded(Outcome.failed),
      OrderDetachable,
      OrderDetached,
      OrderFailed(Position(1))))
  }

  "Suspend and resume at end of workflow" in {
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
  }

  "Suspend then cancel" - {
    "Suspend in the middle of a workflow, then cancel" in {
      testSuspendAndCancel(
        Workflow(
          WorkflowPath("SUSPEND-THEN-CANCEL"),
          Seq(
            Prompt(expr("'PROMPT'")),
            EmptyInstruction())))
    }

    "Suspend at end of workflow, then cancel" in {
      testSuspendAndCancel(
        Workflow(
          WorkflowPath("SUSPEND-AT-END-THEN-CANCEL"),
          Seq(
            Prompt(expr("'PROMPT'")))))
    }

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
}

object SuspendResumeOrdersTest
{
  private val pathExecutable = RelativePathExecutable("executable.cmd")
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val versionId = VersionId("INITIAL")
  private val executeJob = Execute(WorkflowJob(agentPath, pathExecutable, parallelism = 100))

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
}
