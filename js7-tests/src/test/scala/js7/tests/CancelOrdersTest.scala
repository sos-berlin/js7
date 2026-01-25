package js7.tests

import cats.effect.{IO, Resource}
import cats.syntax.traverse.*
import js7.agent.RunningAgent
import js7.base.catsutils.Environment.TaggedResource
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.silly.SillySigner
import js7.base.eventbus.{EventPublisher, StandardEventBus}
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.Processes.runProcess
import js7.base.io.process.{Pid, ProcessSignal, ReturnCode}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.{isUnix, isWindows}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.WaitForCondition.{retryUntil, waitForCondition}
import js7.base.time.{Timestamp, WaitForCondition}
import js7.base.utils.Atomic
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.Problems.{CancelStartedOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode.{FreshOrStarted, Kill}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerCommand.{CancelOrders, ControlWorkflow, Response, ResumeOrder, SuspendOrders}
import js7.data.controller.ControllerState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.item.VersionId
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderBroken, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderPrompted, OrderResumed, OrderRetrying, OrderStarted, OrderStateReset, OrderStdWritten, OrderStdoutWritten, OrderSuspended, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent, OrderTerminated}
import js7.data.order.{FreshOrder, HistoricOutcome, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.problems.CannotResumeOrderProblem
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.{NamedValue, StringConstant, expr, exprFun}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{BreakOrder, Execute, Fail, Fork, If, Prompt, Retry, Sleep, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.processkiller.ProcessKiller.TestChildProcessTerminated
import js7.proxy.data.event.EventAndState
import js7.tests.CancelOrdersTest.*
import js7.tests.jobs.{EmptyJob, FailingJob}
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class CancelOrdersTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.job.execution.sigkill-delay = 30s
    js7.order.stdout-stderr.delay = 10ms
    """

  private lazy val testBus = StandardEventBus[TestChildProcessTerminated]()

  override protected def agentTestWiring = RunningAgent.TestWiring(
    envResources = Seq(TaggedResource(Resource.eval(IO:
      testBus: EventPublisher[TestChildProcessTerminated]))))

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(singleJobWorkflow, sigkillDelayWorkflow, sigkillImmediatelyWorkflow,
    twoJobsWorkflow, forkJoinIfFailedWorkflow, forkWorkflow, promptingWorkflow)

  "Cancel a fresh order" in:
    val order = FreshOrder(OrderId("🔹"), singleJobWorkflow.path,
      scheduledFor = Some(Timestamp.now + 99.seconds))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderAttached](_.key == order.id)
    execCmd:
      CancelOrders(Set(order.id), CancellationMode.FreshOnly)
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(onlyRelevantEvents(eventWatch.eventsByKey[OrderEvent](order.id)) == Vector(
      OrderCancellationMarked(CancellationMode.FreshOnly),
      OrderCancelled))

  "Cancel a finishing order" in:
    val order = FreshOrder(OrderId("🔸"), singleJobWorkflow.path, Map("sleep" -> 1))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    execCmd:
      CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())
    assert(eventWatch.await[OrderTerminated](_.key == order.id).head.value.event == OrderFinished())

    val events = eventWatch.eventsByKey[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten])
    assert(onlyRelevantEvents(events) == Vector(
      OrderProcessingStarted(subagentId),
      OrderCancellationMarked(CancellationMode.FreshOrStarted()),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderFinished()))

  "Cancelling (mode=FreshOnly) a started order is not possible" in:
    val order = FreshOrder(OrderId("♠️"), twoJobsWorkflow.path, Map("sleep" -> 5))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    // Controller knows, the order has started
    assert(controller.api.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOnly)).await(99.seconds) ==
      Left(CancelStartedOrderProblem(OrderId("♠️"))))
    controller.api
      .executeCommand:
        CancelOrders(Set(order.id), CancellationMode.FreshOrStarted(Some(CancellationMode.Kill())))
      .await(99.seconds).orThrow
    eventWatch.await[OrderTerminated](_.key == order.id)

  "Cancel a started order between two jobs" in:
    val order = FreshOrder(OrderId("♣️"), twoJobsWorkflow.path, Map("sleep" -> 1))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    execCmd:
      CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())
    eventWatch.await[OrderCancelled](_.key == order.id)

    val events = eventWatch.eventsByKey[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten])
    assert(onlyRelevantEvents(events) == Vector(
      OrderProcessingStarted(subagentId),
      OrderCancellationMarked(CancellationMode.FreshOrStarted()),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderCancelled))

  "Cancel an order and the first job" in:
    val order = FreshOrder(OrderId("🍊"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)), immediately = false)

  "Cancel an order but not the first job" in:
    val order = FreshOrder(OrderId("🔶"), twoJobsWorkflow.path, Map("sleep" -> 1))
    testCancel(order, Some(twoJobsWorkflow.id /: Position(1)), immediately = false,
      expectedEvents = mode => Vector(
        OrderProcessingStarted(subagentId),
        OrderCancellationMarked(mode),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(OrderOutcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderCancelled))

  "Cancel an order and the currently running job" in:
    val order = FreshOrder(OrderId("🔷"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, None, immediately = false)

  "Cancel an order and a certain running job with SIGKILL" in:
    val order = FreshOrder(OrderId("🟦"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)),
      immediately = true)

  if isUnix then
    "Cancel with sigkillDelay" in:
      val order = FreshOrder(OrderId("🟩"), sigkillDelayWorkflow.path)
      val t = now
      testCancel(order, None, awaitTrapping = true, immediately = false,
        mode => Vector(
          OrderProcessingStarted(subagentId),
          OrderCancellationMarked(mode),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(OrderOutcome.Killed(OrderOutcome.Failed(NamedValues.rc(ReturnCode(SIGKILL))))),
          OrderProcessingKilled,
          OrderCancelled))
      assert(t.elapsed > sigkillDelay)

    "Cancel with sigkillDelay=0s" in:
      val order = FreshOrder(OrderId("🟧"), sigkillImmediatelyWorkflow.path)
      testCancel(order, None, awaitTrapping = true, immediately = false,
        mode => Vector(
          OrderProcessingStarted(subagentId),
          OrderCancellationMarked(mode),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(OrderOutcome.Killed(OrderOutcome.Failed(NamedValues.rc(ReturnCode(SIGKILL))))),
          OrderProcessingKilled,
          OrderCancelled))
  end if

  private def testCancelFirstJob(order: FreshOrder, workflowPosition: Option[WorkflowPosition],
    immediately: Boolean)
  : Unit =
    testCancel(order, workflowPosition, immediately = immediately,
      expectedEvents = mode => Vector(
        OrderProcessingStarted(subagentId),
        OrderCancellationMarked(mode),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(
          if isWindows then
            OrderOutcome.Killed(OrderOutcome.Failed.rc(1))
          else
            OrderOutcome.killed(if immediately then SIGKILL else SIGTERM)),
        OrderProcessingKilled,
        OrderCancelled))

  "Cancel a forking order and kill job" in:
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.path, Map("sleep" -> 1))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")

    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))
    execCmd:
      CancelOrders(Set(order.id), mode)
    eventWatch.await[OrderCancelled](_.key == order.id)

    assert(controller.eventWatch
      .allKeyedEvents[OrderEvent]
      .filter(_.key.string.startsWith("FORK"))
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.arguments,
          scheduledFor = order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Vector("🥕" -> OrderId("FORK|🥕"))),
        OrderId("FORK|🥕") <-: OrderAttachable(agentPath),
        OrderId("FORK|🥕") <-: OrderAttached(agentPath),
        OrderId("FORK|🥕") <-: OrderProcessingStarted(subagentId),
        OrderId("FORK") <-: OrderCancellationMarked(mode),
        OrderId("FORK|🥕") <-: OrderProcessed(OrderOutcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK|🥕") <-: OrderDetachable,
        OrderId("FORK|🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(OrderOutcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderCancelled))

  "Cancel a forked child order and kill job" in:
    val order = FreshOrder(OrderId("CANCEL-CHILD"), forkJoinIfFailedWorkflow.path, Map("sleep" -> 1))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")
    sleep(100.ms)  // Try to avoid "killed before start"

    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))
    execCmd:
      CancelOrders(Set(order.id  / "🥕"), mode)
    eventWatch.await[OrderCancelled](_.key == order.id / "🥕")

    assert(controller.eventWatch
      .allKeyedEvents[OrderEvent]
      .filter(_.key.string.startsWith("CANCEL-CHILD"))
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("CANCEL-CHILD") <-: OrderAdded(forkJoinIfFailedWorkflow.id, order.arguments,
          scheduledFor = order.scheduledFor),
        OrderId("CANCEL-CHILD") <-: OrderStarted,
        OrderId("CANCEL-CHILD") <-: OrderForked(Vector(
          "🥕" -> OrderId("CANCEL-CHILD|🥕"))),
        OrderId("CANCEL-CHILD|🥕") <-: OrderAttachable(agentPath),
        OrderId("CANCEL-CHILD|🥕") <-: OrderAttached(agentPath),
        OrderId("CANCEL-CHILD|🥕") <-: OrderProcessingStarted(subagentId),
        OrderId("CANCEL-CHILD|🥕") <-: OrderCancellationMarked(mode),
        OrderId("CANCEL-CHILD|🥕") <-: OrderCancellationMarkedOnAgent,
        OrderId("CANCEL-CHILD|🥕") <-: OrderProcessed(
          if isWindows then
            OrderOutcome.Killed(OrderOutcome.Failed.rc(1))
          else
            OrderOutcome.killed(SIGTERM)),
        OrderId("CANCEL-CHILD|🥕") <-: OrderProcessingKilled,
        OrderId("CANCEL-CHILD|🥕") <-: OrderDetachable,
        OrderId("CANCEL-CHILD|🥕") <-: OrderDetached,
        OrderId("CANCEL-CHILD|🥕") <-: OrderCancelled,
        OrderId("CANCEL-CHILD") <-: OrderJoined(
          OrderOutcome.Failed(Some("Order:CANCEL-CHILD|🥕 has been cancelled"))),
        OrderId("CANCEL-CHILD") <-: OrderFailed(Position(0))))

  "FIX JS-2069: Cancel a suspended forked child Order" in:
    val workflow = Workflow.of(
      WorkflowPath("FORK-SUSPENDED"),
      Fork.of(
        "🥕" -> Workflow.of(
          EmptyJob.execute(agentPath))),
      EmptyJob.execute(agentPath))
    withItem(workflow) { workflow =>
      // Let the child order suspend
      controller.api
        .executeCommand(
          ControlWorkflow(workflow.id, addBreakpoints = Set(Position(0) / "fork+🥕" % 0)))
        .await(99.s)
        .orThrow

      val orderId = OrderId("🔔")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      controller.eventWatch.await[OrderSuspended](_.key == orderId / "🥕")
      execCmd:
        CancelOrders(Set(orderId / "🥕"))

      val events = controller.eventWatch.await[OrderTerminated](_.key == orderId)
      assert(events.head.value.event.isInstanceOf[OrderFailed])
      assert(controllerState.idToOrder(orderId).lastOutcome ==
        OrderOutcome.Failed(Some("Order:🔔|🥕 has been cancelled")))
    }

  "A canceled Order is not resumable" in:
    val order = FreshOrder(OrderId("⬛"), promptingWorkflow.path)
    controller.addOrderBlocking(order)
    eventWatch.await[OrderPrompted](_.key == order.id)

    execCmd:
      CancelOrders(Seq(order.id))
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controllerState.idToOrder(order.id).isState[Order.Cancelled])

    assert(controller.api
      .executeCommand(
        ResumeOrder(order.id, position = Some(Position(1)), asSucceeded = true))
      .await(99.s) == Left(CannotResumeOrderProblem))

    assert(controllerState.idToOrder(order.id).historicOutcomes.isEmpty)

    assert(eventWatch.eventsByKey[OrderEvent](order.id) == Seq(
      OrderAdded(promptingWorkflow.id, order.arguments, scheduledFor = order.scheduledFor),
      OrderStarted,
      OrderPrompted(StringValue("PROMPT")),
      OrderStateReset,
      OrderCancelled))

  //"Cancellation of a BlockingInternalJob is ignored" in
  // see  BlockingInternalJobTest

  private def testCancel(order: FreshOrder, workflowPosition: Option[WorkflowPosition],
    awaitTrapping: Boolean = false,
    immediately: Boolean,
    expectedEvents: CancellationMode => Seq[OrderEvent])
  : Unit =
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    eventWatch.await[OrderStdoutWritten]:
      case KeyedEvent(order.id, OrderStdoutWritten(chunk)) if chunk.startsWith("READY") => true
      case _ => false
    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill(
      immediately = immediately,
      workflowPosition)))
    execCmd:
      CancelOrders(Set(order.id), mode)
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(onlyRelevantEvents(
      eventWatch.eventsByKey[OrderEvent](order.id)
        .filterNot(_.isInstanceOf[OrderStdWritten])) ==
      expectedEvents(mode))

  "Cancel unknown order" in:
    assert(controller.api.executeCommand(
      CancelOrders(Set(OrderId("UNKNOWN")), CancellationMode.FreshOnly)
    ).await(99.seconds) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))

  "Cancel multiple orders with Batch" in:
    val orders = for i <- 1 to 3 yield
      FreshOrder(
        OrderId(i.toString),
        singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    for o <- orders do controller.addOrderBlocking(o)
    for o <- orders do eventWatch.await[OrderAttached](_.key == o.id)
    val response = execCmd:
      CancelOrders(orders.map(_.id), CancellationMode.FreshOnly)
    assert(response == Response.Accepted)
    for o <- orders do eventWatch.await[OrderCancelled](_.key == o.id)

  if isUnix then "Cancel a script trapping SIGTERM and exiting with 0" in:
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

      controller.api
        .executeCommand:
          CancelOrders(Seq(orderId), CancellationMode.kill())
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
        .collect { case KeyedEvent(`orderId`, event) => event }
      assert(onlyRelevantEvents(events) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("SLEEP\n"),
        OrderCancellationMarked(CancellationMode.kill()),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(OrderOutcome.Killed(OrderOutcome.succeededRC0)),
        OrderProcessingKilled,
        OrderCancelled))

  if isUnix then "Cancel a script having a SIGTERM trap writing to stdout" in:
    val name = "TRAP-STDOUT"
    val orderId = OrderId(name)
    val workflow = Workflow(
      WorkflowPath(name),
      Seq(Execute(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |
            |onSIGTERM() {
            |  # Send some lines to (unbuffered?) stdout — JS7 must read these lines
            |  echo "TRAP 1"
            |  echo "TRAP 2"
            |  echo "TRAP 3"
            |  exit
            |}
            |
            |trap onSIGTERM SIGTERM
            |echo "READY"
            |for i in {0..99}; do
            |  sleep 0.1
            |done
            |""".stripMargin)))))

    withItem(workflow): workflow =>
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

      controller.api
        .executeCommand(
          CancelOrders(Seq(orderId), CancellationMode.kill()))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
        .collect { case KeyedEvent(`orderId`, event) => event }
      assert(onlyRelevantEvents(events) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("READY\n"),
        OrderCancellationMarked(CancellationMode.kill()),
        OrderCancellationMarkedOnAgent,
        OrderStdoutWritten(
          """TRAP 1
            |TRAP 2
            |TRAP 3
            |""".stripMargin),
        OrderProcessed(OrderOutcome.Killed(OrderOutcome.succeededRC0)),
        OrderProcessingKilled,
        OrderCancelled))

  if isUnix then "Cancel a script waiting properly for its child process, forwarding SIGTERM" in:
    val name = "TRAP-CHILD"
    val orderId = OrderId(name)
    val workflow = Workflow(
      WorkflowPath(name),
      Seq(Execute(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |
            |if [ "${1-}" == "-child" ]; then
            |  # Sometimes, the echo does not take effect: trap "wait; echo CHILD EXIT" EXIT
            |  trap wait EXIT
            |  trap "echo CHILD SIGTERM; exit" SIGTERM
            |  for i in {0..1111}; do
            |    sleep 0.1
            |  done
            |else
            |  trap wait EXIT
            |  "$0" -child &
            |  trap 'kill -TERM $!' SIGTERM
            |  echo "READY"
            |fi
            |""".stripMargin)))))

    withItem(workflow): workflow =>
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

      controller.api
        .executeCommand(
          CancelOrders(Seq(orderId), CancellationMode.kill()))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
        .collect { case KeyedEvent(`orderId`, event) => event }
      assert(onlyRelevantEvents(events) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("READY\n"),
        OrderCancellationMarked(FreshOrStarted(Some(Kill(false, None)))),
        OrderCancellationMarkedOnAgent,
        OrderStdoutWritten("CHILD SIGTERM\n"),
          // Sometimes, the echo "CHILD EXIT" does not take effect ???
          //"""CHILD SIGTERM
          //  |CHILD EXIT
          //  |""".stripMargin),
        OrderProcessed(OrderOutcome.killed(SIGTERM)),
        OrderProcessingKilled,
        OrderCancelled))

  if isUnix then "Cancel a script waiting for its child process" in:
    // The child processes are not killed, but cut off from stdout and stderr.
    val name = "EXIT-TRAP"
    val orderId = OrderId(name)
    val workflow = Workflow(
      WorkflowPath(name),
      Seq(Execute(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |
            |sleep 111 &
            |trap "wait; exit" EXIT SIGTERM
            |echo READY
            |""".stripMargin),
        sigkillDelay = Some(500.ms)))))

    withItem(workflow): workflow =>
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

      sleep(500.ms)
      controller.api
        .executeCommand:
          CancelOrders(Seq(orderId), CancellationMode.kill())
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
        .collect { case KeyedEvent(`orderId`, event) => event }
      assert(onlyRelevantEvents(events) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("READY\n"),
        OrderCancellationMarked(FreshOrStarted(Some(Kill(false,None)))),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(OrderOutcome.killed(SIGKILL)),
        OrderProcessingKilled,
        OrderCancelled))

  "Cancel a SIGTERMed script with a still running child process" - {
    "Without traps" in:
      unixOnly:
        run("TERMINATED-SCRIPT", "", SIGTERM)

    "With traps" in:
      unixOnly:
        run("TERMINATED-SCRIPT-TRAPPED", """
          |trap "wait && exit 143" SIGTERM  # 15+128
          |trap "rc=$? && wait && exit $?" EXIT
          |""".stripMargin, SIGKILL)

    def run(name: String, traps: String, expectedSignal: ProcessSignal): Unit =
      // The child processes are not killed, but cut off from stdout and stderr.
      val orderId = OrderId(name)
      val workflow = Workflow(
        WorkflowPath(name),
        Seq(Execute(WorkflowJob(
          agentPath,
            ShellScriptExecutable(
              """#!/usr/bin/env bash
                |set -euo pipefail
                |""".stripMargin +
                traps + """
                |sleep 111 &
                |echo READY
                |for i in {0..99}; do
                |  sleep 0.1
                |done
                |""".stripMargin),
          sigkillDelay = Some(500.ms)))))

      withItem(workflow): workflow =>
        val eventId = eventWatch.lastAddedEventId
        controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

        sleep(500.ms)
        controller.api
          .executeCommand:
            CancelOrders(Seq(orderId), CancellationMode.kill())
          .await(99.s).orThrow
        eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

        val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
          .collect { case KeyedEvent(`orderId`, event) => event }
        assert(onlyRelevantEvents(events) == Seq(
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("READY\n"),
          OrderCancellationMarked(CancellationMode.kill()),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(OrderOutcome.killed(expectedSignal)),
          OrderProcessingKilled,
          OrderCancelled))
  }

  if isUnix then "Sleep in another script" - {
    "Without traps" in:
      run("FOREGROUND", "", SIGTERM)

    "Withs traps" in:
      run("FOREGROUND-TRAPPED",
        """trap "wait && exit 143" SIGTERM  # 15+128
          |trap "rc=$? && wait && exit $?" EXIT
          |""".stripMargin,
        SIGKILL)

    def run(name: String, traps: String, expectedSignal: ProcessSignal): Unit =
      // The child processes are not killed, but cut off from stdout and stderr.
      val orderId = OrderId(name)
      val workflow = Workflow(
        WorkflowPath(name),
        Seq(Execute(WorkflowJob(
          agentPath,
          ShellScriptExecutable(
           """#!/usr/bin/env bash
              |set -euo pipefail
              |""".stripMargin +
             traps + """
              |if [ "${1-}" == "-child" ]; then
              |  sleep 120
              |else
              |  echo READY
              |  "$0" -child
              |fi
              |""".stripMargin),
          sigkillDelay = Some(500.ms)))))

      withItem(workflow): workflow =>
        val eventId = eventWatch.lastAddedEventId
        controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

        sleep(500.ms)
        controller.api
          .executeCommand(
            CancelOrders(Seq(orderId), CancellationMode.kill()))
          .await(99.s).orThrow
        eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

        val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
          .collect { case KeyedEvent(`orderId`, event) => event }
        assert(onlyRelevantEvents(events) == Seq(
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("READY\n"),
          OrderCancellationMarked(FreshOrStarted(Some(Kill(false,None)))),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(OrderOutcome.killed(expectedSignal)),
          OrderProcessingKilled,
          OrderCancelled))
  }

  if isUnix then "Sleep in a background script and don't wait for it" in:
    // The child processes are not killed, but cut off from stdout and stderr.
    val name = "BACKGROUND"
    val orderId = OrderId(name)
    val workflow = Workflow(
      WorkflowPath(name),
      Seq(Execute(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |trap "wait && exit 7" SIGTERM
            |trap "rc=$? && wait && exit $?" EXIT
            |
            |if [ "${1-}" == "-child" ]; then
            |  sleep 120
            |else
            |  echo READY
            |  "$0" -child &
            |fi
            |""".stripMargin),
        sigkillDelay = Some(500.ms)))))

    withItem(workflow): workflow =>
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](_.key == orderId, after = eventId)

      sleep(500.ms)
      controller.api
        .executeCommand:
          CancelOrders(Seq(orderId), CancellationMode.kill())
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)

      val events = eventWatch.keyedEvents[OrderEvent](_.key == orderId, after = eventId)
        .collect { case KeyedEvent(`orderId`, event) => event }
      assert(onlyRelevantEvents(events) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("READY\n"),
        OrderCancellationMarked(FreshOrStarted(Some(Kill(false,None)))),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(OrderOutcome.killed(SIGKILL)),
        OrderProcessingKilled,
        OrderCancelled))

  "Cancel a Broken Order" in:
    val workflow = Workflow.of(WorkflowPath("BROKEN-WORKFLOW"),
      EmptyJob.execute(agentPath),
      BreakOrder())

    withItem(workflow): workflow =>
      val orderId = OrderId("BROKEN")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderBroken](_.key == orderId)

      execCmd:
        CancelOrders(Seq(orderId))
      eventWatch.await[OrderTerminated](_.key == orderId)
      eventWatch.await[OrderDeleted](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) ==
        Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeeded),
          OrderMoved(Position(1)),
          OrderBroken(None),
          OrderCancellationMarked(),
          OrderDetachable,
          OrderDetached,
          OrderCancelled,
          OrderDeleted))

  "FIX JS-2089 Cancel an Order waiting in Retry instruction at an Agent" in:
    val workflow = Workflow(WorkflowPath("RETRY"), Seq(
      TryInstruction(
        Workflow.of(
          FailingJob.execute(agentPath)),
        Workflow.of(
          Retry()),
        retryDelays = Some(Vector(100.s)))))

    withItem(workflow): workflow =>
      val orderId = OrderId("RETRY")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderRetrying](_.key == orderId)

      execCmd:
        CancelOrders(Seq(orderId))
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId)
        .map {
          case OrderRetrying(Some(_), None) => OrderRetrying()
          case o => o
        } == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderProcessed(FailingJob.outcome),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderRetrying(),
          OrderCancellationMarked(),
          OrderDetachable,
          OrderDetached,
          OrderStateReset,
          OrderCancelled,
          OrderDeleted))

  "Child processes are killed, too" - {
    "SIGTERM" in:
      unixOnly:
        testChildProcessesAreKilled(SIGTERM,
          expectedOutcome = OrderOutcome.Killed(OrderOutcome.Failed(namedValues = Map(
            "returnCode" -> NumberValue(7)))))

    "SIGKILL" in:
      unixOnly:
        testChildProcessesAreKilled(SIGKILL,
          expectedOutcome = OrderOutcome.killed(SIGKILL))

    // TODO Sometimes, ProcessKiller's SIGSTOP kills IntelliJ

    def testChildProcessesAreKilled(signal: ProcessSignal, expectedOutcome: OrderOutcome): Unit =
      // Big value (>4000 under macOS) may fail with
      // - in bash fork with "Resource temporarily not available"
      // - in ProcessHandle.descendants with "cannot allocate memory"
      val numberOfChildren = sys.props.get("test.speed").fold(100)(_.toInt)
      val numberOfSpecialChildren = 5

      val script =
        if isWindows then
          """@echo off
            |start /b ping -n 200 127.0.0.1
            |ping -n 200 127.0.0.1
            |""".stripMargin
        else
          """#!/usr/bin/env bash
            |set -euo pipefail
            |
            |# On SIGTERM, terminate watchedChildren properly:
            |watchedChildren=()
            |killWatchedChildren() {
            |  if [[ ${#watchedChildren[*]} -gt 0 ]]; then
            |    echo kill ${watchedChildren[*]}
            |    kill ${watchedChildren[*]}
            |    # On Linux (AlmaLinux 9), we wait for the termination let the kill take effect (?)
            |    wait ${watchedChildren[*]} || true
            |  fi
            |}
            |trap "echo '🔷Trapped SIGTERM🔷'; killWatchedChildren; exit 7" SIGTERM
            |
            |sleep 111 & pid=$!; echo TO BE KILLED BY TRAP pid=$pid; watchedChildren+=($pid)
            |sleep 111 & pid=$!; echo TO BE KILLED BY TRAP pid=$pid; watchedChildren+=($pid)
            |echo watchedChildren="${watchedChildren[*]}"
            |
            |sh <<'END' &
            |  set -euo pipefail
            |  # Ignore SIGTERM, sleep will not be killed because started by the trap
            |  trap "sleep 111" SIGTERM
            |  echo SIGTERM sleep 111 pid=$$
            |  for i in {1..»numberOfNestedChilds«}; do
            |    sh <<<"
            |      echo SLEEP-2-$i pid=\$\$
            |      /bin/sleep 204
            |    " &
            |  done
            |  /bin/sleep 203
            |END
            |
            |/bin/sleep 202 &
            |echo SLEEP pid=$!
            |echo pid=$$
            |for i in {1..2000}; do sleep 0.1; done
            |
            |""".stripMargin
            .replace(
              "»numberOfNestedChilds«",
              (numberOfChildren - numberOfSpecialChildren).toString)
      val workflow = Workflow(WorkflowPath(s"$signal-CHILD-PROCESSES"), Seq(
        Execute:
          WorkflowJob(agentPath, ShellScriptExecutable(script))))

      withItem(workflow): workflow =>
        val pidRegex = ".*pid=([0-9]+).*".r
        val killedByTrapRegex = "TO BE KILLED BY TRAP pid=([0-9]+).*".r
        val eventId = eventWatch.lastAddedEventId
        val orderId = OrderId(s"$signal-CHILD-PROCESSES")
        controller.api.addOrder(FreshOrder(orderId, workflow.path))
          .await(99.s).orThrow


        /// Wait until all child processes are running ///
        // pids: All started child Pids (detected by "pid=..." in stdout)
        // toBeKilledByTrapPids: Pids of children which the script will kill on SIGTERM
        val (pids, toBeKilledByTrapPids) = controller.api
          .eventAndStateStream(fromEventId = Some(eventId))
          .collect:
            case EventAndState(Stamped(_, ms, KeyedEvent(`orderId`, event: OrderEvent)), _, _) =>
              Timestamp.ofEpochMilli(ms) -> event
          .map(_._2)
          .evalTap:
            case e: OrderTerminated =>
              IO.unlessA(e.isInstanceOf[OrderFailed] &&
                controllerState.idToOrder(orderId).lastOutcome ==
                  OrderOutcome.Killed(OrderOutcome.Failed(Map(
                    "returnCode" -> NumberValue(ReturnCode(signal).number))))):
                IO.raiseError(RuntimeException(s"Unexpected $e"))
            case _ => IO.unit
          .takeWhile(e => !e.isInstanceOf[OrderTerminated])
          .collect:
            case event: OrderStdWritten => event.chunk
          .through(fs2.text.lines)
          .scan((Set.empty[Pid], Set.empty[Pid])):
            case ((pids, killedByScriptPids), line) =>
              logger.info(s"> $line")
              val startedPids = pidRegex.findAllMatchIn(line)
                .flatMap(_.subgroups.map(o => Pid(o.toLong)))

              val addedToBeKilledByTrapPids = killedByTrapRegex.findAllMatchIn(line)
                .flatMap(_.subgroups.map(o => Pid(o.toLong)))
              (pids ++ startedPids) -> (killedByScriptPids ++ addedToBeKilledByTrapPids)
          .evalTap((pids, kPids) => IO:
            pids.requireUniqueness
            kPids.requireUniqueness)
          .takeThrough(_._1.size < numberOfChildren)
          .compile
          .last.map(_.get)
          .await(99.s)

        assert(pids.size == numberOfChildren && toBeKilledByTrapPids.size == 2)

        val cancelledAt = Deadline.now

        signal match
          case SIGTERM =>
            // SubagentProcessKiller detects termination of descendant processes,
            // to avoid killing of possibly reused PIDs.
            val terminatedBeforeSigkill = Atomic(Set.empty[Pid])
            testBus.subscribe[TestChildProcessTerminated]: event =>
              if toBeKilledByTrapPids(event.pid) then
                terminatedBeforeSigkill.updateAndGet(_ + event.pid)

            execCmd:
              CancelOrders(Seq(orderId), CancellationMode.kill())

            val t = Deadline.now

            withClue(s"Waiting for termination of child processes killed by a trap: "):
              retryUntil(30.s, 10.ms):
                assert(terminatedBeforeSigkill.get == toBeKilledByTrapPids)

            // Now, the main process has terminated, but its child processes are still running
            sleepUntil(t + 1.s)

            // OrderProcess does not terminate due to still open stdout
            assert(eventWatch.eventsByKey[OrderTerminated](orderId, after = eventId).isEmpty)

            // Now let kill the remembered child processes (despite the main process has terminated)
            execCmd:
              CancelOrders(Seq(orderId), CancellationMode.kill(immediately = true))

          case SIGKILL =>
            execCmd:
              CancelOrders(Seq(orderId), CancellationMode.kill(immediately = signal == SIGKILL))
        end match

        val event = eventWatch.await[OrderTerminated](_.key == orderId, after = eventId)
          .head.value.event
        assert(event == OrderCancelled &&
          controllerState.idToOrder(orderId).lastOutcome == expectedOutcome)
        logger.info(s"⏱️ Cancellation took ${cancelledAt.elapsed.pretty}")

        var runningChildren: Seq[(Pid, ProcessHandle)] = Nil
        waitForCondition(5.s, 10.ms):
          runningChildren = pids.toSeq.flatMap: pid =>
            ProcessHandle.of(pid.number).toScala.map(pid -> _)
          runningChildren.isEmpty

        if runningChildren.nonEmpty then
          runningChildren.foreach: (pid, processHandle) =>
            logger.error(s"Not killed or zombie process: $pid ${
              processHandle.info.commandLine.toScala.getOrElse("")}")

          def failMsg =
            s"Some processes have not been killed: ${runningChildren.map(_._1).mkString(" ")}"

          if !isUnix then
            fail(failMsg)
          else
            val psOutput = runProcess:
              s"ps -o pid,ppid,stat,command ${runningChildren.map(_._1.number).mkString(" ")}"
            logger.error(psOutput)
            if psOutput.split("\n").tail.count(_.contains("<defunct>")) < runningChildren.size then
              fail(s"$failMsg\n$psOutput")
            else
              // Under Docker with AlmaLinux 9, zombies are always left !!!
              // Maybe PID 1 (the start script) could handle this.
              logger.error(s"Killed child processes are left as zombies:\n$psOutput")
              info(s"🔥 Killed child processes are left as zombies:\n$psOutput")
    end testChildProcessesAreKilled
  }

  "returnCode and named values after caught killed script execution" in :
    val workflow = Workflow(WorkflowPath("CAUGHT"), Seq(
      TryInstruction(
        Workflow.of:
          Execute:
            WorkflowJob(
              agentPath,
              ShellScriptExecutable:
                if isWindows then
                  s"""@echo off
                     |echo Start
                     |echo result=Hello >>"%JS7_RETURN_VALUES%"
                     |ping -n 11 127.0.0.1 >nul
                     |""".stripMargin
                else
                  s"""#!/usr/bin/env bash
                     |set -euo pipefail
                     |echo Start
                     |echo "result=Hello" >>"$$JS7_RETURN_VALUES"
                     |i=100
                     |while [ $$i -ge 0 ]; do sleep 0.1; done
                     |""".stripMargin),
        Workflow.of:
          If(expr"$$returnCode == 0"):
            Fail(Some(StringConstant:
              "💥 $returnCode == 0")))))

    withItem(workflow): workflow =>
      val orderId = OrderId("CAUGHT")
      controller.api.addOrder:
        FreshOrder(orderId, workflowPath = workflow.path)
      .await(99.s).orThrow
      eventWatch.awaitNext[OrderStdoutWritten](_.key == orderId)

      execCmd:
        SuspendOrders(Seq(orderId), SuspensionMode.kill)
      eventWatch.awaitNext[OrderSuspended](_.key == orderId)

      execCmd:
        ResumeOrder(orderId, restartKilledJob = Some(false))
      eventWatch.awaitNext[OrderTerminated](_.key == orderId)

      val events = eventWatch.eventsByKey[OrderEvent](orderId)
      assert(events ==
        Vector(
          OrderAdded(workflow.id),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(subagentId),
          OrderStdoutWritten("Start\n"),
          OrderSuspensionMarked(SuspensionMode.kill),
          OrderSuspensionMarkedOnAgent,
          OrderProcessed(OrderOutcome.Killed(OrderOutcome.Failed(Map(
            "returnCode" -> NumberValue(sigtermReturnCode))))),
          OrderProcessingKilled,
          OrderDetachable,
          OrderDetached,
          OrderSuspended,
          OrderResumed(),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderMoved(Position(1)),
          OrderFinished()))
      assert(controllerState.idToOrder(orderId).historicOutcomes ==
        Vector(
          HistoricOutcome(Position(0) / "try+0" % 0, OrderOutcome.Killed(OrderOutcome.Failed(Map(
            "returnCode" -> NumberValue(sigtermReturnCode))))),
          HistoricOutcome(Position(0) / "catch+0" % 0, OrderOutcome.Caught)))

  "Cancel via Expression predicate" in:
    controller.resetLastWatchedEventId()

    val orderIds = Seq(OrderId("EXPR-A1"), OrderId("EXPR-A2"), OrderId("EXPR-B"))
    for orderId <- orderIds do
      addOrder:
        FreshOrder(orderId, singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.seconds))
      controller.awaitNextKey[OrderAttached](orderId)

    execCmd:
      CancelOrders(
        expr"orderId => substring($$orderId ++ '      ', 0, 6) == 'EXPR-A'",
        CancellationMode.FreshOnly)
    controller.await[OrderCancelled](_.key == orderIds(0))
    controller.await[OrderCancelled](_.key == orderIds(1))

    execCmd:
      CancelOrders(Seq(orderIds(2)))
    for orderId <- orderIds do
      controller.await[OrderTerminated](_.key == orderId)

  "Speed test Expression with real ControllerState" in :
    withItem(Workflow.empty): workflow =>
      val wp = workflow.id /: Position(0)
      val n = 100_000
      val orders = (1 to n).view.map: i =>
        Order(OrderId(i.toString), wp, Order.Fresh())
      .toVector
      val expr = expr"substring(orderId, 0, 1) != '7'"
      (1 to (if isIntelliJIdea then 3 else 1)).foreach: _ =>
        val t = Deadline.now
        val result = orders.traverse: order =>
          controllerState.toOrderScope(order).flatMap: scope =>
            expr.eval(using scope)
        val elapsed = t.elapsed
        logInfo(s"With $n orders: ${itemsPerSecondString(elapsed, orders.size, "evaluations")}")
        assert(result.rightAs(()) == Checked.unit)

  private def logInfo(line: String) =
    logger.info(line)
    if !isIntelliJIdea then info(line)


/** Speed tests without Engine under optimal laboratory conditions. */
final class CancelOrdersExprFunctionSpeedTest extends OurTestSuite:
  private val logger = Logger[this.type]

  "Speed test ExprFunction" in:
    val orderIds = (1 to 1_000_000).view.map(_.toString).toVector
    val fun = exprFun"orderId => substring($$orderId, 0, 1) != '7'"
    (1 to (if isIntelliJIdea then 20 else 1)).foreach: _ =>
      val t = Deadline.now
      val result = orderIds.traverse: orderId =>
        fun.eval(orderId)(using Scope.empty)
      val elapsed = t.elapsed
      logInfo(itemsPerSecondString(elapsed, orderIds.size, "evaluations"))
      assert(result.rightAs(()) == Checked.unit)

  "Speed test Expression" in:
    val itemSigner = ControllerState.toItemSigner(SillySigner.Default)
    val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Seq(Sleep(expr"1")))
    val controllerState = ControllerState.empty
      .applyKeyedEvents(Seq(
        NoKey <-: VersionAdded(workflow.id.versionId),
        NoKey <-: VersionedItemAdded(itemSigner.sign(workflow))))
      .orThrow
    val wp = workflow.id /: Position(0)
    val orders = (1 to 1_000_000).view.map: i =>
      Order(OrderId(i.toString), wp, Order.Fresh())
    .toVector
    val expr = expr"substring(orderId, 0, 1) != '7'"
    (1 to (if isIntelliJIdea then 20 else 1)).foreach: _ =>
      val t = Deadline.now
      val result = orders.traverse: order =>
        controllerState.toOrderScope(order).flatMap: scope =>
          expr.eval(using scope)
      val elapsed = t.elapsed
      logInfo(itemsPerSecondString(elapsed, orders.size, "evaluations"))
      assert(result.rightAs(()) == Checked.unit)

  private def logInfo(line: String) =
    logger.info(line)
    if !isIntelliJIdea then info(line)


object CancelOrdersTest:

  private val logger = Logger[this.type]
  private val sigtermReturnCode = if isWindows then 1 else 128 + SIGTERM.number

  private val sleepingExecutable = ShellScriptExecutable(
    if isWindows then
       """@echo off
         |echo READY
         |ping -n %SLEEP% 127.0.0.1
         |ping -n 2 127.0.0.1
         |""".stripMargin
    else
       """#!/usr/bin/env bash
         |set -euo pipefail
         |echo "READY"
         |i=0
         |while [ $i -lt $SLEEP ]; do
         |  i=$(($i + 1))
         |  for j in {0..9}; do
         |    sleep 0.1
         |  done
         |done
         |""".stripMargin,
    Map("SLEEP" -> NamedValue("sleep")))
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val versionId = VersionId("INITIAL")

  private val singleJobWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, sleepingExecutable)))

  private val sigtermIgnoringExecutable = ShellScriptExecutable(
    """#!/usr/bin/env bash
      |set -euo pipefail
      |trap "" SIGTERM
      |echo "READY"
      |for i in {0..99}; do
      |  sleep 0.1
      |done
      |""".stripMargin)

  private val sigkillDelay = 500.ms
  private val sigkillDelayWorkflow = Workflow.of(
    WorkflowPath("SIGKILL-DELAY") ~ versionId,
    Execute(WorkflowJob(agentPath, sigtermIgnoringExecutable, sigkillDelay = Some(sigkillDelay))))

  private val sigkillImmediatelyWorkflow = Workflow.of(
    WorkflowPath("SIGKILL-DELAY-0") ~ versionId,
    Execute(WorkflowJob(agentPath, sigtermIgnoringExecutable, sigkillDelay = Some(0.s))))

  private val twoJobsWorkflow = Workflow.of(
    WorkflowPath("TWO") ~ versionId,
    Execute(WorkflowJob(agentPath, sleepingExecutable)),
    Execute(WorkflowJob(agentPath, sleepingExecutable)))

  private val forkJoinIfFailedWorkflow = Workflow.of(
    WorkflowPath("FORK-JOIN-IF-FAILED") ~ versionId,
    Fork(
      Vector(
        "🥕" -> Workflow.of(
          Execute(WorkflowJob(agentPath, sleepingExecutable)))),
      joinIfFailed = true),
    Execute(WorkflowJob(agentPath, sleepingExecutable)))

  private val forkWorkflow = Workflow.of(
    WorkflowPath("FORK") ~ versionId,
    Fork.of(
      "🥕" -> Workflow.of(
        Execute(WorkflowJob(agentPath, sleepingExecutable)))),
    Execute(WorkflowJob(agentPath, sleepingExecutable)))

  private val promptingWorkflow = Workflow.of(
    WorkflowPath("WORKFLOW") ~ versionId,
    Prompt(expr"'PROMPT'"),
    EmptyJob.execute(agentPath))

  private def onlyRelevantEvents(events: Seq[OrderEvent]): Seq[OrderEvent] =
    events.filter:
      case _: OrderAdded => false
      case _: OrderStarted => false
      case _: OrderAttachable => false
      case _: OrderAttached => false
      case _: OrderDetachable => false
      case _: OrderDetached => false
      case _ => true
