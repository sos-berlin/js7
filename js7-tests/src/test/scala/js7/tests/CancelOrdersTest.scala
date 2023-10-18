package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.{ProcessSignal, ReturnCode}
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.{isUnix, isWindows}
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.Problems.{CancelStartedOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.command.CancellationMode.{FreshOrStarted, Kill}
import js7.data.controller.ControllerCommand.{CancelOrders, ControlWorkflow, Response, ResumeOrder}
import js7.data.event.KeyedEvent
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion}
import js7.data.item.VersionId
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderBroken, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOperationCancelled, OrderOutcomeAdded, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderPrompted, OrderRetrying, OrderStarted, OrderStdWritten, OrderStdoutWritten, OrderSuspended, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.problems.CannotResumeOrderProblem
import js7.data.value.Value.convenience.*
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{BreakOrder, Execute, Fail, Fork, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CancelOrdersTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class CancelOrdersTest extends OurTestSuite with ControllerAgentForScalaTest
with BlockingItemUpdater
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.job.execution.sigkill-delay = 30s
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(singleJobWorkflow, sigkillDelayWorkflow, sigkillImmediatelyWorkflow,
    twoJobsWorkflow, forkJoinIfFailedWorkflow, forkWorkflow, promptingWorkflow)

  "Cancel a fresh order" in {
    val order = FreshOrder(OrderId("🔹"), singleJobWorkflow.path,
      scheduledFor = Some(Timestamp.now + 99.seconds))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderAttached](_.key == order.id)
    controller.api.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOnly))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(onlyRelevantEvents(eventWatch.eventsByKey[OrderEvent](order.id)) == Vector(
      OrderCancellationMarked(CancellationMode.FreshOnly),
      OrderCancelled))
  }

  "Cancel a finishing order" in {
    val order = FreshOrder(OrderId("🔸"), singleJobWorkflow.path, Map("sleep" -> 1))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.api.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted()))
      .await(99.seconds).orThrow
    assert(eventWatch.await[OrderTerminated](_.key == order.id).head.value.event == OrderFinished())

    val events = eventWatch.eventsByKey[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten])
    assert(onlyRelevantEvents(events) == Vector(
      OrderProcessingStarted(subagentId),
      OrderCancellationMarked(CancellationMode.FreshOrStarted()),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderFinished()))
  }

  "Cancelling (mode=FreshOnly) a started order is not possible" in {
    val order = FreshOrder(OrderId("♠️"), twoJobsWorkflow.path, Map("sleep" -> 5))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)

    // Controller knows, the order has started
    assert(controller.api.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOnly)).await(99.seconds) ==
      Left(CancelStartedOrderProblem(OrderId("♠️"))))
    controller.api
      .executeCommand(
        CancelOrders(Set(order.id), CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))))
      .await(99.seconds).orThrow
    eventWatch.await[OrderTerminated](_.key == order.id)
  }

  "Cancel a started order between two jobs" in {
    val order = FreshOrder(OrderId("♣️"), twoJobsWorkflow.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.api.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted()))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)

    val events = eventWatch.eventsByKey[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten])
    assert(onlyRelevantEvents(events) == Vector(
      OrderProcessingStarted(subagentId),
      OrderCancellationMarked(CancellationMode.FreshOrStarted()),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderCancelled))
  }

  "Cancel an order and the first job" in {
    val order = FreshOrder(OrderId("🔻"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)), immediately = false)
  }

  "Cancel an order but not the first job" in {
    val order = FreshOrder(OrderId("🔶"), twoJobsWorkflow.path, Map("sleep" -> 2))
    testCancel(order, Some(twoJobsWorkflow.id /: Position(1)), immediately = false,
      expectedEvents = mode => Vector(
        OrderProcessingStarted(subagentId),
        OrderCancellationMarked(mode),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderCancelled))
  }

  "Cancel an order and the currently running job" in {
    val order = FreshOrder(OrderId("🔷"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, None, immediately = false)
  }

  "Cancel an order and a certain running job with SIGKILL" in {
    val order = FreshOrder(OrderId("🟦"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)),
      immediately = true)
  }

  if isUnix then {
    "Cancel with sigkillDelay" in {
      val order = FreshOrder(OrderId("🟩"), sigkillDelayWorkflow.path)
      val t = now
      testCancel(order, None, awaitTrapping = true, immediately = false,
        mode => Vector(
          OrderProcessingStarted(subagentId),
          OrderCancellationMarked(mode),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(Outcome.Killed(Outcome.Failed(NamedValues.rc(ReturnCode(SIGKILL))))),
          OrderProcessingKilled,
          OrderCancelled))
      assert(t.elapsed > sigkillDelay)
    }

    "Cancel with sigkillDelay=0s" in {
      val order = FreshOrder(OrderId("🟧"), sigkillImmediatelyWorkflow.path)
      testCancel(order, None, awaitTrapping = true, immediately = false,
        mode => Vector(
          OrderProcessingStarted(subagentId),
          OrderCancellationMarked(mode),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(Outcome.Killed(Outcome.Failed(NamedValues.rc(ReturnCode(SIGKILL))))),
          OrderProcessingKilled,
          OrderCancelled))
    }
  }

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
            Outcome.Killed(Outcome.Failed.rc(1))
          else
            Outcome.killed(if immediately then SIGKILL else SIGTERM)),
        OrderProcessingKilled,
        OrderCancelled))

  "Cancel a forking order and kill job" in {
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")

    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))
    controller.api.executeCommand(CancelOrders(Set(order.id), mode))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)

    assert(controller.eventWatch
      .allKeyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.arguments, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Vector("🥕" -> OrderId("FORK|🥕"))),
        OrderId("FORK|🥕") <-: OrderAttachable(agentPath),
        OrderId("FORK|🥕") <-: OrderAttached(agentPath),
        OrderId("FORK|🥕") <-: OrderProcessingStarted(subagentId),
        OrderId("FORK") <-: OrderCancellationMarked(mode),
        OrderId("FORK|🥕") <-: OrderProcessed(Outcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK|🥕") <-: OrderDetachable,
        OrderId("FORK|🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(Outcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderCancelled))
  }

  "Cancel a forked child order and kill job" in {
    val order = FreshOrder(OrderId("CANCEL-CHILD"), forkJoinIfFailedWorkflow.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")
    sleep(100.ms)  // Try to avoid "killed before start"

    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))
    controller.api.executeCommand(CancelOrders(Set(order.id  / "🥕"), mode))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id / "🥕")

    assert(controller.eventWatch
      .allKeyedEvents[OrderEvent]
      .filter(_.key.string startsWith "CANCEL-CHILD")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("CANCEL-CHILD") <-: OrderAdded(forkJoinIfFailedWorkflow.id, order.arguments,
          order.scheduledFor),
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
            Outcome.Killed(Outcome.Failed.rc(1))
          else
            Outcome.killed(SIGTERM)),
        OrderId("CANCEL-CHILD|🥕") <-: OrderProcessingKilled,
        OrderId("CANCEL-CHILD|🥕") <-: OrderDetachable,
        OrderId("CANCEL-CHILD|🥕") <-: OrderDetached,
        OrderId("CANCEL-CHILD|🥕") <-: OrderCancelled,
        OrderId("CANCEL-CHILD") <-: OrderJoined(
          Outcome.Failed(Some("Order:CANCEL-CHILD|🥕 has been cancelled"))),
        OrderId("CANCEL-CHILD") <-: OrderFailed(Position(0))))
  }

  "FIX JS-2069: Cancel a suspended forked child Order" in {
    val workflow = Workflow.of(
      WorkflowPath("FORK-SUSPENDED"),
      Fork.of(
        "🥕" -> Workflow.of(
          EmptyJob.execute(agentPath))),
      EmptyJob.execute(agentPath))
    withTemporaryItem(workflow) { workflow =>
      // Let the child order suspend
      controller.api
        .executeCommand(
          ControlWorkflow(workflow.id, addBreakpoints = Set(Position(0) / "fork+🥕" % 0)))
        .await(99.s)
        .orThrow

      val orderId = OrderId("🔔")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      controller.eventWatch.await[OrderSuspended](_.key == orderId / "🥕")
      controller.api.executeCommand(CancelOrders(Set(orderId / "🥕"))).await(99.s).orThrow

      val events = controller.eventWatch.await[OrderTerminated](_.key == orderId)
      assert(events.head.value.event.isInstanceOf[OrderFailed])
      assert(controllerState.idToOrder(orderId).lastOutcome ==
        Outcome.Failed(Some("Order:🔔|🥕 has been cancelled")))
    }
  }

  "A canceled Order is not resumable" in {
    val order = FreshOrder(OrderId("⬛"), promptingWorkflow.path)
    controller.addOrderBlocking(order)
    eventWatch.await[OrderPrompted](_.key == order.id)

    controller.api.executeCommand(CancelOrders(Seq(order.id))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controllerState.idToOrder(order.id).isState[Order.Cancelled])

    assert(controller.api
      .executeCommand(
        ResumeOrder(order.id, position = Some(Position(1)), asSucceeded = true))
      .await(99.s) == Left(CannotResumeOrderProblem))

    assert(controllerState.idToOrder(order.id).historicOutcomes.isEmpty)

    assert(eventWatch.eventsByKey[OrderEvent](order.id) == Seq(
      OrderAdded(promptingWorkflow.id, order.arguments, order.scheduledFor),
      OrderStarted,
      OrderPrompted(StringValue("PROMPT")),
      OrderOperationCancelled,
      OrderCancelled))
  }

  private def testCancel(order: FreshOrder, workflowPosition: Option[WorkflowPosition],
    awaitTrapping: Boolean = false,
    immediately: Boolean,
    expectedEvents: CancellationMode => Seq[OrderEvent])
  : Unit = {
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    eventWatch.await[OrderStdoutWritten] {
      case KeyedEvent(order.id, OrderStdoutWritten(chunk)) if chunk startsWith "READY" => true
      case _ => false
    }
    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill(
      immediately = immediately,
      workflowPosition)))
    controller.api.executeCommand(CancelOrders(Set(order.id), mode))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(onlyRelevantEvents(
      eventWatch.eventsByKey[OrderEvent](order.id)
        .filterNot(_.isInstanceOf[OrderStdWritten])) ==
      expectedEvents(mode))
  }

  "Cancel unknown order" in {
    assert(controller.api.executeCommand(
      CancelOrders(Set(OrderId("UNKNOWN")), CancellationMode.FreshOnly)
    ).await(99.seconds) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Cancel multiple orders with Batch" in {
    val orders = for i <- 1 to 3 yield
      FreshOrder(
        OrderId(i.toString),
        singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    for o <- orders do controller.addOrderBlocking(o)
    for o <- orders do eventWatch.await[OrderAttached](_.key == o.id)
    val response = controller.api.executeCommand(
      CancelOrders(orders.map(_.id), CancellationMode.FreshOnly)
    ).await(99.seconds).orThrow
    assert(response == Response.Accepted)
    for o <- orders do eventWatch.await[OrderCancelled](_.key == o.id)
  }

  if isUnix then "Cancel a script having a SIGTERM trap writing to stdout" in {
    val name = "TRAP-STDOUT"
    val orderId = OrderId(name)
    val v = VersionId(name)
    val workflow = Workflow(
      WorkflowPath(name) ~ v,
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
    addWorkflow(workflow)

    val eventId = eventWatch.lastAddedEventId
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderStdoutWritten](after = eventId)

    controller.api
      .executeCommand(
        CancelOrders(Seq(orderId), CancellationMode.kill()))
      .await(99.s).orThrow
    eventWatch.await[OrderTerminated](after = eventId)

    val events = eventWatch.keyedEvents[OrderEvent](after = eventId)
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
      OrderProcessed(Outcome.Killed(Outcome.succeededRC0)),
      OrderProcessingKilled,
      OrderCancelled))
  }

  if isUnix then "Cancel a script waiting properly for its child process, forwarding SIGTERM" in {
    val name = "TRAP-CHILD"
    val orderId = OrderId(name)
    val v = VersionId(name)
    val workflow = Workflow(
      WorkflowPath(name) ~ v,
      Seq(Execute(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |
            |if [ "$1" == "-child" ]; then
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
    addWorkflow(workflow)

    val eventId = eventWatch.lastAddedEventId
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderStdoutWritten](after = eventId)

    controller.api
      .executeCommand(
        CancelOrders(Seq(orderId), CancellationMode.kill()))
      .await(99.s).orThrow
    eventWatch.await[OrderTerminated](after = eventId)

    val events = eventWatch.keyedEvents[OrderEvent](after = eventId)
      .collect { case KeyedEvent(`orderId`, event) => event }
    assert(onlyRelevantEvents(events) == Seq(
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("READY\n"),
      OrderCancellationMarked(FreshOrStarted(Some(Kill(false,None)))),
      OrderCancellationMarkedOnAgent,
      OrderStdoutWritten("CHILD SIGTERM\n"),
        // Sometimes, the echo "CHILD EXIT" does not take effect ???
        //"""CHILD SIGTERM
        //  |CHILD EXIT
        //  |""".stripMargin),
      OrderProcessed(Outcome.killed(SIGTERM)),
      OrderProcessingKilled,
      OrderCancelled))
  }

  if isUnix then "Cancel a script waiting for its child process" in {
    // The child processes are not killed, but cut off from stdout and stderr.
    val name = "EXIT-TRAP"
    val orderId = OrderId(name)
    val v = VersionId(name)
    val workflow = Workflow(
      WorkflowPath(name) ~ v,
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
    addWorkflow(workflow)

    val eventId = eventWatch.lastAddedEventId
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderStdoutWritten](after = eventId)

    sleep(500.ms)
    controller.api
      .executeCommand(
        CancelOrders(Seq(orderId), CancellationMode.kill()))
      .await(99.s).orThrow
    eventWatch.await[OrderTerminated](after = eventId)

    val events = eventWatch.keyedEvents[OrderEvent](after = eventId)
      .collect { case KeyedEvent(`orderId`, event) => event }
    assert(onlyRelevantEvents(events) == Seq(
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("READY\n"),
      OrderCancellationMarked(FreshOrStarted(Some(Kill(false,None)))),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(Outcome.killed(SIGKILL)),
      OrderProcessingKilled,
      OrderCancelled))
  }

  if isUnix then "Cancel a SIGTERMed script with a still running child process" - {
    "Without traps" in {
      run("TERMINATED-SCRIPT", "", SIGTERM)
    }

    "With traps" in {
      run("TERMINATED-SCRIPT-TRAPPED", """
        |trap "wait && exit 143" SIGTERM  # 15+128
        |trap "rc=$? && wait && exit $?" EXIT
        |""".stripMargin, SIGKILL)
    }

    def run(name: String, traps: String, expectedSignal: ProcessSignal): Unit = {
      // The child processes are not killed, but cut off from stdout and stderr.
      val orderId = OrderId(name)
      val v = VersionId(name)
      val workflow = Workflow(
        WorkflowPath(name) ~ v,
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
      addWorkflow(workflow)

      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](after = eventId)

      sleep(500.ms)
      controller.api
        .executeCommand(
          CancelOrders(Seq(orderId), CancellationMode.kill()))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](after = eventId)

      val events = eventWatch.keyedEvents[OrderEvent](after = eventId)
        .collect { case KeyedEvent(`orderId`, event) => event }
      assert(onlyRelevantEvents(events) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("READY\n"),
        OrderCancellationMarked(CancellationMode.kill()),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(Outcome.killed(expectedSignal)),
        OrderProcessingKilled,
        OrderCancelled))
    }
  }

  if isUnix then "Sleep in another script" - {
    "Without traps" in {
      run("FOREGROUND", "", SIGTERM)
    }

    "Withs traps" in {
      run("FOREGROUND-TRAPPED",
        """trap "wait && exit 143" SIGTERM  # 15+128
          |trap "rc=$? && wait && exit $?" EXIT
          |""".stripMargin,
        SIGKILL)
    }
    def run(name: String, traps: String, expectedSignal: ProcessSignal): Unit = {
      // The child processes are not killed, but cut off from stdout and stderr.
      val orderId = OrderId(name)
      val v = VersionId(name)
      val workflow = Workflow(
        WorkflowPath(name) ~ v,
        Seq(Execute(WorkflowJob(
          agentPath,
          ShellScriptExecutable(
           """#!/usr/bin/env bash
              |set -euo pipefail
              |""".stripMargin +
             traps + """
              |if [ "$1" == "-child" ]; then
              |  sleep 120
              |else
              |  echo READY
              |  "$0" -child
              |fi
              |""".stripMargin),
          sigkillDelay = Some(500.ms)))))
      addWorkflow(workflow)

      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStdoutWritten](after = eventId)

      sleep(500.ms)
      controller.api
        .executeCommand(
          CancelOrders(Seq(orderId), CancellationMode.kill()))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](after = eventId)

      val events = eventWatch.keyedEvents[OrderEvent](after = eventId)
        .collect { case KeyedEvent(`orderId`, event) => event }
      assert(onlyRelevantEvents(events) == Seq(
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("READY\n"),
        OrderCancellationMarked(FreshOrStarted(Some(Kill(false,None)))),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(Outcome.killed(expectedSignal)),
        OrderProcessingKilled,
        OrderCancelled))
    }
  }

  if isUnix then "Sleep in a background script and do not wait for it" in {
    // The child processes are not killed, but cut off from stdout and stderr.
    val name = "BACKGROUND"
    val orderId = OrderId(name)
    val v = VersionId(name)
    val workflow = Workflow(
      WorkflowPath(name) ~ v,
      Seq(Execute(WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |trap "wait && exit 143" SIGTERM  # 15+128
            |trap "rc=$? && wait && exit $?" EXIT
            |
            |if [ "$1" == "-child" ]; then
            |  sleep 120
            |else
            |  echo READY
            |  "$0" -child &
            |fi
            |""".stripMargin),
        sigkillDelay = Some(500.ms)))))
    addWorkflow(workflow)

    val eventId = eventWatch.lastAddedEventId
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderStdoutWritten](after = eventId)

    sleep(1.s)
    controller.api
      .executeCommand(
        CancelOrders(Seq(orderId), CancellationMode.kill()))
      .await(99.s).orThrow
    eventWatch.await[OrderTerminated](after = eventId)

    val events = eventWatch.keyedEvents[OrderEvent](after = eventId)
      .collect { case KeyedEvent(`orderId`, event) => event }
    assert(onlyRelevantEvents(events) == Seq(
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("READY\n"),
      OrderCancellationMarked(FreshOrStarted(Some(Kill(false,None)))),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(Outcome.killed(SIGKILL)),
      OrderProcessingKilled,
      OrderCancelled))
  }

  "Cancel a Broken Order" in {
    val workflow = Workflow.of(WorkflowPath("BROKEN-WORKFLOW"),
      EmptyJob.execute(agentPath),
      BreakOrder())
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("BROKEN")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderBroken](_.key == orderId)

      controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)
      eventWatch.await[OrderDeleted](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) ==
        Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted(Some(subagentId)),
          OrderProcessed(Outcome.succeeded),
          OrderMoved(Position(1)),
          OrderBroken(None),
          OrderCancellationMarked(),
          OrderDetachable,
          OrderDetached,
          OrderCancelled,
          OrderDeleted))
    }
  }

  "FIX JS-2089 Cancel an Order waiting in Retry instruction at an Agent" in {
    val workflow = Workflow(WorkflowPath("RETRY"), Seq(
      TryInstruction(
        Workflow.of(
          Fail()),
        Workflow.of(
          Retry()),
        retryDelays = Some(Vector(100.s)))))

    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("RETRY")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderRetrying](_.key == orderId)

      controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId)
        .map {
          case OrderRetrying(movedTo, Some(_)) => OrderRetrying(movedTo)
          case o => o
        } == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderStarted,
          OrderOutcomeAdded(Outcome.failed),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderRetrying(Position(0) / "try+1" % 0),
          OrderCancelled,
          OrderDeleted))
    }
  }

  private def addWorkflow(workflow: Workflow): Unit = {
    controller.api
      .updateItems(Observable(
        AddVersion(workflow.id.versionId),
        AddOrChangeSigned(directoryProvider.itemSigner.toSignedString(workflow))))
      .await(99.s).orThrow
  }
}

object CancelOrdersTest
{
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

  private val sigkillDelay = 1.s
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
    Prompt(expr("'PROMPT'")),
    EmptyJob.execute(agentPath))

  private def onlyRelevantEvents(events: Seq[OrderEvent]): Seq[OrderEvent] =
    events.filter {
      case _: OrderAdded => false
      case _: OrderStarted => false
      case _: OrderAttachable => false
      case _: OrderAttached => false
      case _: OrderDetachable => false
      case _: OrderDetached => false
      case _ => true
    }
}
