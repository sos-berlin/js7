package js7.tests.subagent

import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.isSubtypeOf
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCancelled, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.Problems.ProcessKilledDueToSubagentShutdownProblem
import js7.data.subagent.SubagentCommand
import js7.data.subagent.SubagentItemStateEvent.{SubagentShutdown, SubagentShutdownStarted}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.convenience.given
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.{Execute, If, StickySubagent}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.language.implicitConversions
import scala.util.Random

final class SubagentShutdownTest extends OurTestSuite, SubagentTester:

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(bareSubagentItem)

  private val localSubagentId = toLocalSubagentId(agentPath)

  "After Subagent shutdown, process is repeated at a different Subagent" in:
    runSubagent(bareSubagentItem): subagent =>
      withItem(
        Workflow.of:
          Execute.shellScript(agentPath, processLimit = 100):
            """echo STARTED
              |sleep 100
              |""".stripMargin
      ): workflow =>
        val orderIds = Vector.fill(4):
          val orderId = addOrder(workflow.path)
          controller.eventWatch.awaitNextKey[OrderProcessingStarted](orderId)
          controller.eventWatch.awaitNextKey[OrderStdoutWritten](orderId)
          orderId

        subagent.executeCommandForTest:
          SubagentCommand.ShutDown(Some(SIGKILL))
        .await(99.s).orThrow

        val eventId = controller.awaitNextKey[SubagentShutdownStarted](bareSubagentId).head.eventId
        controller.awaitKey[OrderProcessed](orderIds(1), after = eventId)
        controller.awaitKey[OrderProcessed](orderIds(3), after = eventId)
        controller.awaitNextKey[SubagentShutdown](bareSubagentId)
        controller.awaitKey[OrderStdoutWritten](orderIds(1), after = eventId)
        controller.awaitKey[OrderStdoutWritten](orderIds(3), after = eventId)
        execCmd:
          CancelOrders(orderIds, CancellationMode.kill(immediately = true))

        orderIds.foreach: orderId =>
          controller.awaitKey[OrderTerminated](orderId)

        // Both orderIds(1) and orderIds(3) are repeated at the local Subagent, because the
        // bare Subagent has been shutdown.
        List(orderIds(1), orderIds(3)).foreach: orderId =>
          assert(controller.eventsByKey[OrderEvent](orderId) == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderAttachable(agentPath),
            OrderAttached(agentPath),
            OrderStarted,
            OrderProcessingStarted(Some(bareSubagentId)),
            OrderStdoutWritten("STARTED\n"),
            OrderProcessed(OrderOutcome.processLost(ProcessKilledDueToSubagentShutdownProblem(OrderOutcome.killed(SIGKILL)))),
            OrderMoved(Position(0)),
            OrderProcessingStarted(Some(localSubagentId)),
            OrderStdoutWritten("STARTED\n"),
            OrderCancellationMarked(CancellationMode.kill(immediately = true)),
            OrderCancellationMarkedOnAgent,
            OrderProcessed(OrderOutcome.killed(SIGKILL)),
            OrderProcessingKilled,
            OrderDetachable,
            OrderDetached,
            OrderCancelled,
            OrderDeleted))

  "Test with StickySubagent" in:
    val n = 10
    val sleep = if isIntelliJIdea then 5.s else 1.s
    val orderIds = (1 to n).map(i => OrderId(s"ORDER-$i"))

    enableSubagents(localSubagentId -> false, bareSubagentId -> true)

    runSubagent(bareSubagentItem): subagent =>
      withItem(
        Workflow.of:
          StickySubagent(agentPath):
            If(true):
              Workflow.of(
                Execute.shellScript(agentPath, processLimit = n):
                  ":",
                Execute.shellScript(agentPath,
                  defaultArguments = Map("sleepArg" -> expr"$$sleep"),
                  env = Map("SLEEP" -> expr"$$sleepArg"),
                  processLimit = n
                ):
                  """echo STARTED
                    |sleep $SLEEP
                    |""".stripMargin)
      ): workflow =>
        controller.api.addOrders:
          orderIds.map: orderId =>
            FreshOrder(orderId, workflow.path,
              Map("sleep" -> Random.nextLong(1 + sleep.toMillis).ms.toBigDecimalSeconds),
              deleteWhenTerminated = true)
        .await(99.s).orThrow

        for orderId <- orderIds do
          controller.eventWatch.awaitKey[OrderProcessed](orderId)
          controller.eventWatch.awaitKey[OrderStdoutWritten](orderId)

        subagent.executeCommandForTest:
          SubagentCommand.ShutDown()
        .await(99.s).orThrow
        controller.awaitNextKey[SubagentShutdown](bareSubagentId)

        for orderId <- orderIds do
          val events = controller.eventWatch.awaitKey[OrderTerminated](orderId).map(_.value)
          assert(events.exists(_.isSubtypeOf[OrderFinished]))


object SubagentShutdownTest:
  private val logger = Logger[this.type]
