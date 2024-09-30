package js7.tests.order

import cats.effect.IO
import cats.effect.std.Semaphore
import cats.effect.unsafe.IORuntime
import cats.syntax.functor.*
import java.nio.file.Files.{createTempFile, deleteIfExists}
import java.time.LocalDate
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.touchFile
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.{isUnix, isWindows}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, DailyPeriod, Timestamp}
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentReady
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerCommand.{AddOrder, AnswerOrderPrompt, Batch, CancelOrders, GoOrder, Response, ResumeOrder, ResumeOrders, SuspendOrders}
import js7.data.event.KeyedEvent
import js7.data.item.VersionId
import js7.data.job.{RelativePathExecutable, ShellScriptExecutable}
import js7.data.order.Order.DelayingRetry
import js7.data.order.OrderEvent.OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, InsertHistoricOutcome, ReplaceHistoricOutcome}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAwoke, OrderCancellationMarkedOnAgent, OrderCancelled, OrderCaught, OrderCycleFinished, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderGoMarked, OrderGoes, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderResumed, OrderResumptionMarked, OrderRetrying, OrderStarted, OrderStdWritten, OrderStdoutWritten, OrderSuspended, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent, OrderTerminated}
import js7.data.order.{FreshOrder, HistoricOutcome, Order, OrderEvent, OrderId, OrderMark, OrderOutcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Cycle, EmptyInstruction, Execute, Fail, Fork, Prompt, Retry, Schedule, TryInstruction}
import js7.data.workflow.position.BranchId.{Try_, catch_, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.{EmptyJob, FailingJob, SemaphoreJob}
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

  "Order is detached immediately in some states" - {
    "Order.Fresh" in:
      val eventId = eventWatch.lastAddedEventId
      val workflow = Workflow.of(WorkflowPath("FRESH"), EmptyJob.execute(agentPath))
      withItem(workflow): workflow =>
        val orderId = OrderId("FRESH")
        val scheduledFor = Some(Timestamp.now + 100.s)
        controller.api.addOrder:
          FreshOrder(orderId, workflow.path, scheduledFor = scheduledFor, deleteWhenTerminated = true)
        .await(99.s).orThrow
        eventWatch.awaitNext[OrderAttached](_.key == orderId)

        execCmd(SuspendOrders(Set(orderId)))
        eventWatch.awaitNext[OrderSuspended](_.key == orderId)

        execCmd(ResumeOrders(Set(orderId)))
        eventWatch.awaitNext[OrderResumed](_.key == orderId)
        eventWatch.awaitNext[OrderAttached](_.key == orderId)

        // ResumeOrders command expects a suspended or suspending order
        assert:
          controller.api.executeCommand(ResumeOrders(Set(orderId))).await(99.s) == Left:
            CannotResumeOrderProblem

        execCmd(GoOrder(orderId, Position(0)))
        eventWatch.awaitNext[OrderFinished](_.key == orderId)
        assert(eventWatch.eventsByKey[OrderEvent](orderId, after = eventId) == Seq(
          OrderAdded(workflow.id, scheduledFor = scheduledFor, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderSuspensionMarked(),
          OrderDetachable,
          OrderDetached,
          OrderSuspended,

          OrderResumed(),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),

          OrderGoMarked(Position(0)),
          OrderGoes,
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(1)),
          OrderDetachable,
          OrderDetached,
          OrderFinished(),
          OrderDeleted))

    "Order.Ready, waiting for job admission time (nothing special)" in:
      eventWatch.resetLastWatchedEventId()
      val calendar = Calendar.jocStandard(CalendarPath("CALENDAR"))
      val workflow = Workflow.of(
        WorkflowPath("JOB-ADMISSION"),
        EmptyJob.execute(agentPath),
        Execute(WorkflowJob(
          agentPath,
          EmptyJob.executable(),
          admissionTimeScheme = Some(AdmissionTimeScheme(Seq(
            DailyPeriod(24 * 3600 - 1, 1.s))))))/*,
        calendarPath = Some(calendar.path)*/)
      withItems((calendar, workflow)): (_, workflow) =>
        val orderId = OrderId(s"#2024-09-25#JOB-ADMISSION")
        addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderProcessed](_.key == orderId)
        eventWatch.awaitNext[OrderMoved](_.key == orderId)
        sleep(100.ms) // nothing should happen

        execCmd(SuspendOrders(Set(orderId)))
        eventWatch.awaitNext[OrderDetached](_.key == orderId)
        eventWatch.awaitNext[OrderSuspended](_.key == orderId)
        assert(controllerState.idToOrder(orderId).isState[Order.Ready])

        execCmd(CancelOrders(Set(orderId)))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

    "Order.DelayingRetry" in:
      eventWatch.resetLastWatchedEventId()
      val workflow = Workflow.of(WorkflowPath("DELAYED-AFTER-ERROR"),
        TryInstruction(
          Workflow.of:
            FailingJob.execute(agentPath),
          Workflow.of:
            Retry(),
          retryDelays = Some(Vector(100.s)),
          maxTries = Some(2)))
      withItem(workflow): workflow =>
        val orderId = OrderId(workflow.path.string)
        addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderRetrying](_.key == orderId)

        execCmd:
          SuspendOrders(Set(orderId))
        eventWatch.awaitNext[OrderDetached](_.key == orderId)

        assert(controllerState.idToOrder(orderId).isState[DelayingRetry])
        execCmd:
          GoOrder(orderId, controllerState.idToOrder(orderId).position)
        eventWatch.awaitNext[OrderGoes](_.key == orderId)
        eventWatch.awaitNext[OrderSuspended](_.key == orderId)


        execCmd:
          ResumeOrders(Set(orderId))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert:
          eventWatch.eventsByKey[OrderEvent](orderId).mapOrKeep:
            case e: OrderRetrying => e.copy(delayedUntil = Some(Timestamp.Epoch))
          == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderMoved(Position(0) / "try+0" % 0),
            OrderAttachable(agentPath),
            OrderAttached(agentPath),
            OrderStarted,
            OrderProcessingStarted(subagentId),
            OrderProcessed(FailingJob.outcome),
            OrderCaught(Position(0) / "catch+0" % 0),
            OrderRetrying(Some(Timestamp.Epoch)),

            OrderSuspensionMarked(),
            OrderDetachable,
            OrderDetached,

            OrderGoes,
            OrderAwoke,
            OrderMoved(Position(0) / "try+1" % 0),

            OrderSuspended,
            OrderResumed(),
            OrderAttachable(agentPath),
            OrderAttached(agentPath),
            OrderProcessingStarted(subagentId),
            OrderProcessed(FailingJob.outcome),
            OrderDetachable,
            OrderDetached,
            OrderFailed(Position(0) / "try+1" % 0))

    "Order.Prompting, with resetState" in:
      eventWatch.resetLastWatchedEventId()
      val workflow = Workflow.of(WorkflowPath("DELAYED-AFTER-ERROR-PROMPTING"),
        Prompt(expr("'PROMPT'")))

      withItem(workflow): workflow =>
        val orderId = OrderId(workflow.path.string)
        addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderPrompted](_.key == orderId)

        execCmd(SuspendOrders(Set(orderId), SuspensionMode(resetState = true)))
        eventWatch.awaitNext[OrderSuspended](_.key == orderId)

        assert(controllerState.idToOrder(orderId).isState[Order.Ready])

        execCmd(ResumeOrders(Set(orderId)))
        eventWatch.awaitNext[OrderPrompted](_.key == orderId)

        execCmd(CancelOrders(Set(orderId)))
        eventWatch.awaitNext[OrderCancelled](_.key == orderId)

    "Order.DelayingRetry, with resetState" in:
      eventWatch.resetLastWatchedEventId()
      val workflow = Workflow.of(WorkflowPath("DELAYED-AFTER-ERROR-RESET-STATE"),
        TryInstruction(
          Workflow.of:
            FailingJob.execute(agentPath),
          Workflow.of:
            Retry(),
          retryDelays = Some(Vector(100.s)),
          maxTries = Some(2)))

      withItem(workflow): workflow =>
        val orderId = OrderId(workflow.path.string)
        addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderRetrying](_.key == orderId)

        execCmd:
          SuspendOrders(Set(orderId), SuspensionMode(resetState = true))
        eventWatch.awaitNext[OrderDetached](_.key == orderId)
        eventWatch.awaitNext[OrderSuspended](_.key == orderId)

        assert(controllerState.idToOrder(orderId).isState[Order.Ready])

        execCmd:
          ResumeOrders(Set(orderId))
        /// Retry is executed again ///
        eventWatch.awaitNext[OrderRetrying](_.key == orderId)

        execCmd:
          GoOrder(orderId, Position(0) / "catch+0" % 0)
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

    "Order.BetweenCycles" in:
      eventWatch.resetLastWatchedEventId()
      val today = LocalDate.now().toString // Test may fail around midnight
      val orderId = OrderId(s"#$today#CYCLE")
      val calendar = Calendar.jocStandard(CalendarPath("CALENDAR"))
      val workflow = Workflow(
        WorkflowPath("CYCLE-PREPARED"),
        Seq:
          Cycle(Schedule.continuous(100.s, limit = Some(2))):
            Workflow.of:
              EmptyJob.execute(agentPath),
        calendarPath = Some(calendar.path))

      withItems((calendar, workflow)): (_, workflow) =>
        addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderCycleFinished](_.key == orderId)

        execCmd:
          SuspendOrders(Set(orderId))
        eventWatch.awaitNext[OrderDetached](_.key == orderId)
        assert(controllerState.idToOrder(orderId).isState[Order.BetweenCycles])

        execCmd:
          GoOrder(orderId, controllerState.idToOrder(orderId).position)
        eventWatch.awaitNext[OrderSuspended](_.key == orderId)

        execCmd:
          ResumeOrders(Set(orderId))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

    "Order.BetweenCycles, with resetState" in:
      eventWatch.resetLastWatchedEventId()
      val today = LocalDate.now().toString // Test may fail around midnight
      val orderId = OrderId(s"#$today#CYCLE-RESET")
      val calendar = Calendar.jocStandard(CalendarPath("CALENDAR-RESET"))
      val workflow = Workflow(
        WorkflowPath("CYCLE-PREPARED-RESET"),
        Seq:
          Cycle(Schedule.continuous(100.s, limit = Some(2))):
            Workflow.of:
              EmptyJob.execute(agentPath),
        calendarPath = Some(calendar.path))

      withItems((calendar, workflow)): (_, workflow) =>
        addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderCycleFinished](_.key == orderId)

        execCmd:
          SuspendOrders(Set(orderId), SuspensionMode(resetState = true))
        eventWatch.awaitNext[OrderDetached](_.key == orderId)
        assert(controllerState.idToOrder(orderId).isState[Order.Ready])

        execCmd:
          ResumeOrders(Set(orderId))
        eventWatch.awaitNext[OrderCycleFinished](_.key == orderId)

        execCmd:
          GoOrder(orderId, controllerState.idToOrder(orderId).position)
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)
  }

  "Order.Processing" - {
    "Order.Processing, last instruction of workflow" in:
      eventWatch.resetLastWatchedEventId()
      val workflow = Workflow.of(WorkflowPath("PROCESSING"),
        OurSemaphoreJob.execute(agentPath))
      withItem(workflow): workflow =>
        val order = FreshOrder(OrderId("PROCESSING"), workflow.path)
        controller.api.addOrder(order).await(99.s).orThrow
        eventWatch.awaitNext[OrderStdoutWritten](_.key == order.id)

        execCmd(SuspendOrders(Set(order.id)))
        eventWatch.awaitNext[OrderSuspensionMarkedOnAgent](_.key == order.id)
        OurSemaphoreJob.continue()
        eventWatch.awaitNext[OrderProcessed](_.key == order.id)
        eventWatch.awaitNext[OrderSuspended](_.key == order.id)

        execCmd:
          ResumeOrders(Set(order.id))
        eventWatch.awaitNext[OrderFinished](_.key == order.id)

        assert(eventWatch.eventsByKey[OrderEvent](order.id) == Seq(
          OrderAdded(workflow.id, order.arguments, order.scheduledFor),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten(OurSemaphoreJob.stdoutLine),
          OrderSuspensionMarked(),
          OrderSuspensionMarkedOnAgent,
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(1)),
          OrderDetachable,
          OrderDetached,
          OrderSuspended,

          OrderResumed(),
          OrderFinished()))

    "Order.Processing, kill the process" in:
      eventWatch.resetLastWatchedEventId()
      val workflow = Workflow.of(WorkflowPath("PROCESSING-KILL"),
        OurSemaphoreJob.execute(agentPath))
      withItem(workflow): workflow =>
        val orderId = OrderId("PROCESSING-KILL")
        addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.awaitNext[OrderProcessingStarted](_.key == orderId)
        eventWatch.awaitNext[OrderStdoutWritten](_.key == orderId)

        execCmd(SuspendOrders(Set(orderId), SuspensionMode.killImmediately))
        eventWatch.awaitNext[OrderSuspended](_.key == orderId)

        OurSemaphoreJob.continue()
        execCmd(ResumeOrders(Set(orderId), asSucceeded = true/*job starts again*/))
        eventWatch.awaitNext[OrderTerminated](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten(OurSemaphoreJob.stdoutLine),
          OrderSuspensionMarked(SuspensionMode.killImmediately),
          OrderSuspensionMarkedOnAgent,
          OrderProcessed(OrderOutcome.Killed(
            if isWindows then
              OrderOutcome.Failed.rc(1)
            else
              OrderOutcome.Failed(Some("Canceled")))),
          OrderProcessingKilled,
          OrderDetachable,
          OrderDetached,
          OrderSuspended,
          OrderResumed(asSucceeded = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten(OurSemaphoreJob.stdoutLine),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(1)),
          OrderDetachable,
          OrderDetached,
          OrderFinished(),
          OrderDeleted))
  }

  "Suspend with kill, trapped with exit 0" in:
    if !isUnix then
      alert("Unix-only test")
    else
      val name = "TRAP-EXIT-0"
      val workflow = Workflow(
        WorkflowPath(name),
        Seq(
          Execute(WorkflowJob(
            agentPath,
            ShellScriptExecutable(
              """#!/usr/bin/env bash
                |set -euo pipefail
                |
                |trap "exit 0" SIGTERM
                |echo SLEEP
                |for i in {0..99}; do
                |  sleep 0.1
                |done
                |""".stripMargin))),
          EmptyJob.execute(agentPath)))

      withItem(workflow): workflow =>
        val eventId = eventWatch.lastAddedEventId
        val orderId = OrderId(name)
        controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

        execCmd:
          SuspendOrders(Seq(orderId), SuspensionMode.kill)
        eventWatch.await[OrderSuspended](_.key == orderId, after = eventId)

        execCmd:
          ResumeOrders(Seq(orderId))

        val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
          .collect { case KeyedEvent(`orderId`, event) => event }
        assert(onlyRelevantEvents(events) == Seq(
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("SLEEP\n"),
          OrderSuspensionMarked(SuspensionMode.kill),
          OrderSuspensionMarkedOnAgent,
          OrderProcessed(OrderOutcome.Killed(OrderOutcome.succeededRC0)),
          OrderProcessingKilled,
          OrderSuspended,
          OrderResumed(),
          OrderFailed(Position(0))))

  "Suspend and resume orders between two jobs" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("♦️"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow

    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    execCmd:
      SuspendOrders(Seq(order.id))
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
    execCmd:
      ResumeOrders(Seq(order.id))
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

    execCmd:
      CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())
    assert(executeCommand(SuspendOrders(Set(order.id))).await(99.s) == Left(CannotSuspendOrderProblem))
    eventWatch.await[OrderCancellationMarkedOnAgent](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderCancelled](_.key == order.id)

  "Suspending a forked order does not suspend child orders" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")

    execCmd:
      SuspendOrders(Set(order.id))
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
    val response = execCmd(Batch:
      for o <- orders yield CorrelIdWrapped(CorrelId.empty, SuspendOrders(Set(o.id))))
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for o <- orders do eventWatch.await[OrderSuspended](_.key == o.id)

  "Resume a still suspending order" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔹"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    execCmd:
      SuspendOrders(Set(order.id))
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)

    execCmd:
      ResumeOrders(Set(order.id))
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

    execCmd:
      SuspendOrders(Set(order.id))
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

    execCmd:
      CancelOrders(Set(order.id))
    eventWatch.await[OrderCancelled](_.key == order.id)

  "Suspend and resume twice on same Agent" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🟧"), twoJobsWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    execCmd:
      SuspendOrders(Set(order.id))
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id)

    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id)
    val eventId = eventWatch.lastAddedEventId

    execCmd:
      ResumeOrder(order.id)
    eventWatch.await[OrderProcessingStarted](_.key == order.id, after = eventId)

    execCmd:
      SuspendOrders(Set(order.id))
    eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == order.id, after = eventId)

    touchFile(triggerFile)
    eventWatch.await[OrderSuspended](_.key == order.id, after = eventId)
    execCmd:
      ResumeOrder(order.id)
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

    execCmd:
      SuspendOrders(Set(order.id))
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

    execCmd:
      CancelOrders(Set(order.id))
    eventWatch.await[OrderCancelled](_.key == order.id)

  "Resume with changed position and changed historic outcomes" in:
    deleteIfExists(triggerFile)
    val order = FreshOrder(OrderId("🔶"), tryWorkflow.path)
    addOrder(order).await(99.s).orThrow
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    execCmd:
      SuspendOrders(Set(order.id))
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

    execCmd:
      ResumeOrder(order.id, Some(newPosition), historicOutcomeOps)
    eventWatch.await[OrderFailed](_.key == order.id)

    assert(eventWatch.eventsByKey[OrderEvent](order.id, after = lastEventId)
      .filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderResumed(Some(newPosition), historicOutcomeOps),
        OrderOutcomeAdded(OrderOutcome.Failed(Some("FAILURE"))),
        OrderCaught(Position(2) / catch_(0) % 0),
        OrderRetrying(),
        OrderMoved(Position(2) / try_(1) % 0, None),
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
    execCmd:
      ResumeOrder(order.id, asSucceeded = true)
    eventWatch.await[OrderFailed](_.key == order.id, after = eventId)
    assert(controllerState.idToOrder(order.id).historicOutcomes == Seq(
      HistoricOutcome(Position(0), OrderOutcome.succeeded),
      HistoricOutcome(Position(1), OrderOutcome.failed),
      HistoricOutcome(Position(1), OrderOutcome.succeeded)/*Resume asSucceeded*/,
      HistoricOutcome(Position(1), OrderOutcome.failed)))

    eventId = eventWatch.lastAddedEventId
    execCmd:
      ResumeOrder(order.id, Some(Position(0)), asSucceeded = true)

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

    withItem(workflow): workflow =>
      val orderId = OrderId("SUSPEND-AT-END")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.await[OrderPrompted](_.key == orderId)

      execCmd:
        SuspendOrders(Seq(orderId))
      execCmd:
        AnswerOrderPrompt(orderId)
      eventWatch.await[OrderSuspended](_.key == orderId)

      execCmd:
        ResumeOrders(Seq(orderId))
      eventWatch.await[OrderDeleted](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderPrompted(StringValue("PROMPT")),
        OrderSuspensionMarked(),
        OrderPromptAnswered(),
        OrderMoved(Position(1)),
        OrderSuspended,
        OrderResumed(),
        OrderFinished(),
        OrderDeleted))

  "Suspend then cancel" - {
    "Suspend in the middle of a workflow, then cancel" in:
      testSuspendAndCancel:
        Workflow(
          WorkflowPath("SUSPEND-THEN-CANCEL"),
          Seq(
            Prompt(expr("'PROMPT'")),
            EmptyInstruction()))

    "Suspend at end of workflow, then cancel" in:
      testSuspendAndCancel:
        Workflow(
          WorkflowPath("SUSPEND-AT-END-THEN-CANCEL"),
          Seq(
            Prompt(expr("'PROMPT'"))))

    def testSuspendAndCancel(workflow: Workflow): Unit =
      withItem(workflow): workflow =>
        val orderId = OrderId(workflow.path.string)
        controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        eventWatch.await[OrderPrompted](_.key == orderId)

        execCmd:
          SuspendOrders(Seq(orderId))
        execCmd:
          AnswerOrderPrompt(orderId)
        eventWatch.await[OrderSuspended](_.key == orderId)

        execCmd:
          CancelOrders(Seq(orderId))
        eventWatch.await[OrderDeleted](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderPrompted(StringValue("PROMPT")),
          OrderSuspensionMarked(SuspensionMode.standard),
          OrderPromptAnswered(),
          OrderMoved(Position(1)),
          OrderSuspended,
          OrderCancelled,
          OrderDeleted))
  }

  // Test moved to RetryTest:
  // "FIX JS-2089 Cancel an Order waiting in Retry instruction at an Agent"

  "Suspend a processing Order that will fail" in:
    // The failed order will not be suspended, because it failed.
    val workflow = Workflow.of(WorkflowPath("FAIL"),
      FailingSemaJob.execute(agentPath),
      EmptyInstruction())
    withItem(workflow): workflow =>
      val orderId = OrderId("SUSPEND-FAILING-JOB")
      var eventId = eventWatch.lastAddedEventId
      execCmd:
        AddOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

      execCmd:
        SuspendOrders(Seq(orderId))
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == orderId, after = eventId)

      FailingSemaJob.semaphore.flatMap(_.release).await(99.s)
      eventWatch.await[OrderFailed](_.key == orderId, after = eventId)

      // OrderFailed event has reset OrderMark.Suspending
      assert(controllerState.idToOrder(orderId).mark == None)

      eventId = eventWatch.lastAddedEventId
      execCmd:
        ResumeOrder(orderId, Some(Position(1)), asSucceeded = true)
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

    withItem(workflow): workflow =>
      val orderId = OrderId("SUSPEND-FORKED-FAILING-JOB")
      val childOrderId = OrderId("SUSPEND-FORKED-FAILING-JOB|BRANCH")
      var eventId = eventWatch.lastAddedEventId
      execCmd:
        AddOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.await[OrderStdoutWritten](_.key == childOrderId, after = eventId)

      execCmd:
        SuspendOrders(Seq(childOrderId))
      eventWatch.await[OrderSuspensionMarkedOnAgent](_.key == childOrderId, after = eventId)

      FailingSemaJob.semaphore.flatMap(_.release).await(99.s)
      eventWatch.await[OrderFailed](_.key == childOrderId, after = eventId)

      // OrderFailed event has reset OrderMark.Suspending
      assert(controllerState.idToOrder(childOrderId).mark == None)

      eventId = eventWatch.lastAddedEventId
      execCmd:
        ResumeOrder(childOrderId, Some(Position(0) / BranchId.fork("BRANCH") % 1),
          asSucceeded = true)
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

    withItem(workflow): workflow =>
      val orderId = OrderId("SUSPEND-FORKED-FAILING-JOB-JOIN-IF-FAILED")
      val childOrderId = OrderId("SUSPEND-FORKED-FAILING-JOB-JOIN-IF-FAILED|BRANCH")
      var eventId = eventWatch.lastAddedEventId
      execCmd:
        AddOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.await[OrderStdoutWritten](_.key == childOrderId, after = eventId)

      execCmd:
        SuspendOrders(Seq(childOrderId))
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
          execCmd:
            ResumeOrder(childOrderId, Some(Position(0) / BranchId.fork("BRANCH") % 1),
              asSucceeded = true)
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

  "Suspend while in Order.DelayAfterError" in:
    val workflow = Workflow.of(
      WorkflowPath("SUSPEND-IN-RETRY"),
      TryInstruction(
        tryWorkflow = Workflow.of(FailingJob.execute(agentPath)),
        catchWorkflow = Workflow.of(Retry()),
        retryDelays = Some(Vector(60.s))))

    withItem(workflow): workflow =>
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("SUSPEND-IN-RETRY")

      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      val orderRetrying = eventWatch.await[OrderRetrying](_.key == orderId, after = eventId)
        .head.value.event
      val until = orderRetrying.delayedUntil.get
      assert(until >= Timestamp.now + 50.s && until <= Timestamp.now + 70.s)

      execCmd(SuspendOrders(Seq(orderId)))
      eventWatch.await[OrderDetached](_.key == orderId, after = eventId)

      assert(controllerState.idToOrder(orderId) == Order(
        orderId,
        workflow.id /: (Position(0) / "catch+0" % 0),
        DelayingRetry(until),
        mark = Some(OrderMark.Suspending()),
        deleteWhenTerminated = true,
        historicOutcomes = Vector(
          HistoricOutcome(Position(0) / "try+0" % 0, FailingJob.outcome),
          HistoricOutcome(Position(0) / "catch+0" % 0, OrderOutcome.succeeded))))

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(0) / "try+0" % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(FailingJob.outcome),
        OrderCaught(Position(0) / "catch+0" % 0),
        OrderRetrying(Some(until)),
        OrderSuspensionMarked(),
        OrderDetachable,
        OrderDetached))

      execCmd(CancelOrders(Seq(orderId)))


object SuspendResumeOrdersTest:

  private val pathExecutable = RelativePathExecutable("executable.cmd")
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val versionId = VersionId("INITIAL")
  private val executeJob = Execute(WorkflowJob(agentPath, pathExecutable, processLimit = 100))

  private def onlyRelevantEvents(events: Seq[OrderEvent]): Seq[OrderEvent] =
    events.filter:
      case _: OrderAdded => false
      case _: OrderStarted => false
      case _: OrderAttachable => false
      case _: OrderAttached => false
      case _: OrderDetachable => false
      case _: OrderDetached => false
      case _ => true

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

  private class OurSemaphoreJob extends SemaphoreJob(OurSemaphoreJob)
  private object OurSemaphoreJob extends SemaphoreJob.Companion[OurSemaphoreJob]

  final class FailingSemaJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess.cancelable:
        step.writeOut("FailingJob\n")
          .*>(FailingSemaJob.semaphore)
          .flatMap(_.acquire)
          .as(OrderOutcome.failed)

  private object FailingSemaJob extends InternalJob.Companion[FailingSemaJob]:
    val semaphore = Semaphore[IO](0).unsafeMemoize
